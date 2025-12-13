import std/[os, osproc, strutils]


proc exec(cmd: string; showProgress = false) =
  if showProgress:
    let exitCode = execShellCmd(cmd)
    if exitCode != 0:
      quit "FAILURE " & cmd & "\n"
  else:
    let (s, exitCode) = execCmdEx(cmd)
    if exitCode != 0:
      quit "FAILURE " & cmd & "\n" & s

proc execExpectFailure(cmd: string; expectedSubstr = "") =
  let (s, exitCode) = execCmdEx(cmd)
  if exitCode == 0:
    quit "EXPECTED FAILURE " & cmd & "\n"
  if expectedSubstr.len > 0 and not s.contains(expectedSubstr):
    quit "UNEXPECTED OUTPUT " & cmd & "\nExpected to contain: " & expectedSubstr & "\nGot:\n" & s


when defined(macosx):
  exec "nim c -r src/nifasm/nifasm tests/hello_darwin.nif && tests/hello_darwin"
elif defined(windows):
  exec "nim c -r src/nifasm/nifasm tests/hello_win64.nif"
  exec "./tests/hello_win64.exe"
else:
  exec "nim c -r src/nifasm/nifasm tests/hello.nif && tests/hello"
  exec "nim c -r src/nifasm/nifasm tests/unique_bind.nif"
  exec "nim c -r src/nifasm/nifasm tests/kill_reuse.nif"
  exec "nim c -r src/nifasm/nifasm tests/kill_reuse_multi.nif"
  exec "nim c -r src/nifasm/nifasm tests/kill_reuse_types.nif"
  execExpectFailure("nim c -r src/nifasm/nifasm tests/double_bind.nif", "Register RAX is already bound to variable 'x.0'")
  execExpectFailure("nim c -r src/nifasm/nifasm tests/triple_bind.nif", "Register RAX is already bound to variable 'x.0'")
  execExpectFailure("nim c -r src/nifasm/nifasm tests/quadruple_bind.nif", "Register RAX is already bound to variable 'x.0'")
  execExpectFailure("nim c -r src/nifasm/nifasm tests/kill_use_after_kill.nif", "Expected variable or register as destination")