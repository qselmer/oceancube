param(
  [Parameter(Mandatory = $true)][string]$Rscript,
  [Parameter(Mandatory = $true)][string]$Worker,
  [Parameter(Mandatory = $true)][string]$ArgumentsFile,
  [Parameter(Mandatory = $true)][string]$MonitorOutput,
  [Parameter(Mandatory = $true)][string]$Stdout,
  [Parameter(Mandatory = $true)][string]$Stderr
)

$ErrorActionPreference = "Stop"
trap {
  Write-Error ("A6 monitor failure at line " + $_.InvocationInfo.ScriptLineNumber + ": " + $_.Exception.Message)
  exit 1
}
$workerArguments = Get-Content -LiteralPath $ArgumentsFile -Encoding UTF8
$startInfo = [System.Diagnostics.ProcessStartInfo]::new()
$startInfo.FileName = $Rscript
$startInfo.UseShellExecute = $false
$startInfo.CreateNoWindow = $true
$startInfo.RedirectStandardOutput = $true
$startInfo.RedirectStandardError = $true
$startInfo.Environment.Clear()
$seenEnvironment = [System.Collections.Generic.HashSet[string]]::new(
  [System.StringComparer]::OrdinalIgnoreCase
)
foreach ($entry in [System.Environment]::GetEnvironmentVariables().GetEnumerator()) {
  $key = [string]$entry.Key
  if ($seenEnvironment.Add($key)) {
    $startInfo.Environment[$key] = [string]$entry.Value
  }
}
$startInfo.ArgumentList.Add("--vanilla")
$startInfo.ArgumentList.Add($Worker)
foreach ($argument in $workerArguments) {
  $startInfo.ArgumentList.Add($argument)
}

$process = [System.Diagnostics.Process]::new()
$process.StartInfo = $startInfo
if (-not $process.Start()) { throw "R worker process did not start" }
$stdoutTask = $process.StandardOutput.ReadToEndAsync()
$stderrTask = $process.StandardError.ReadToEndAsync()
$observedPeak = [int64]0
$observedWorkingSet = [int64]0
while (-not $process.HasExited) {
  $process.Refresh()
  if ([int64]$process.PeakWorkingSet64 -gt $observedPeak) {
    $observedPeak = [int64]$process.PeakWorkingSet64
  }
  if ([int64]$process.WorkingSet64 -gt $observedWorkingSet) {
    $observedWorkingSet = [int64]$process.WorkingSet64
  }
  Start-Sleep -Milliseconds 20
}
$process.WaitForExit()
$stdoutTask.Wait()
$stderrTask.Wait()
[System.IO.File]::WriteAllText($Stdout, $stdoutTask.Result, [System.Text.UTF8Encoding]::new($false))
[System.IO.File]::WriteAllText($Stderr, $stderrTask.Result, [System.Text.UTF8Encoding]::new($false))

$record = [pscustomobject]@{
  exit_code = $process.ExitCode
  peak_rss_bytes = $observedPeak
  sampled_working_set_peak_bytes = $observedWorkingSet
  launcher_pid_equals_worker_pid = $true
  monitor = "System.Diagnostics.Process.PeakWorkingSet64 sampled while handle is live"
  process_isolation = "Rscript --vanilla"
}
$record | Export-Csv -LiteralPath $MonitorOutput -NoTypeInformation -Encoding UTF8
exit $process.ExitCode
