
import std / [parseopt, strutils, os, tables, assertions, streams]
import "../../../nimony/src/lib" / [nifreader, stringviews]
import tags
import x86, elf

const
  Version = "0.1.0"
  Usage = "nifasm - Native NIF Assembler " & Version & """

  (c) 2025 Andreas Rumpf

Usage:
  nifasm [options] file.nif

Options:
  --output:file, -o:file    specify output file name (default: file)
  --help, -h                show this help
  --version, -v             show version
"""

type
  Assembler = object
    r: Reader
    filename: string
    outfile: string
    tagMap: Table[string, TagEnum]
    buf: Buffer
    labelMap: Table[string, LabelId]
    regMap: Table[string, Register]
    
proc initAssembler(filename: string; outfile: string): Assembler =
  result = Assembler(
    r: nifreader.open(filename), 
    filename: filename, 
    outfile: outfile,
    tagMap: initTable[string, TagEnum](),
    buf: initBuffer(),
    labelMap: initTable[string, LabelId](),
    regMap: initTable[string, Register]()
  )
  for t in TagEnum:
    if t != InvalidTagId:
      let (name, _) = TagData[t]
      result.tagMap[name] = t
  
  # Initialize register map
  result.regMap["rax"] = RAX
  result.regMap["rcx"] = RCX
  result.regMap["rdx"] = RDX
  result.regMap["rbx"] = RBX
  result.regMap["rsp"] = RSP
  result.regMap["rbp"] = RBP
  result.regMap["rsi"] = RSI
  result.regMap["rdi"] = RDI
  result.regMap["r8"] = R8
  result.regMap["r9"] = R9
  result.regMap["r10"] = R10
  result.regMap["r11"] = R11
  result.regMap["r12"] = R12
  result.regMap["r13"] = R13
  result.regMap["r14"] = R14
  result.regMap["r15"] = R15

proc close(a: var Assembler) =
  a.r.close()

proc parseTag(a: var Assembler; t: Token): TagEnum =
  if t.tk == ParLe:
    let name = $t.s
    result = a.tagMap.getOrDefault(name, InvalidTagId)
  else:
    result = InvalidTagId

proc getLabel(a: var Assembler; name: string): LabelId =
  if name notin a.labelMap:
    a.labelMap[name] = a.buf.createLabel()
  result = a.labelMap[name]

proc parseReg(a: var Assembler): Register =
  let t = next(a.r)
  # We expect (rax) or just rax?
  # The doc says (rax).
  if t.tk == ParLe:
    let name = $t.s
    if name in a.regMap:
      result = a.regMap[name]
      # consume ParRi
      let endT = next(a.r)
      assert endT.tk == ParRi
    else:
      # Could be (s) or other things?
      quit "Expected register, got: " & name
  elif t.tk == Ident or t.tk == Symbol:
    let name = $t.s
    if name in a.regMap:
      result = a.regMap[name]
    else:
      quit "Expected register, got: " & name
  else:
    quit "Expected register token"

proc compileStmt(a: var Assembler; tag: TagEnum)

proc process(a: var Assembler) =
  while true:
    let t = next(a.r)
    if t.tk == EofToken: break
    if t.tk == ParLe:
      let tag = a.parseTag(t)
      if tag != InvalidTagId:
        a.compileStmt(tag)
      else:
        echo "Warning: Invalid tag at top level: ", t.s

proc compileStmt(a: var Assembler; tag: TagEnum) =
  case tag
  of StmtsTagId:
    while true:
      let t = next(a.r)
      if t.tk == ParRi: break
      if t.tk == ParLe:
        let subTag = a.parseTag(t)
        a.compileStmt(subTag)
  of SyscallTagId:
    a.buf.emitSyscall()
    let endT = next(a.r)
    assert endT.tk == ParRi
  of RetTagId:
    # Instruction (ret)
    a.buf.emitRet()
    let endT = next(a.r)
    assert endT.tk == ParRi
  of MovTagId:
    # (mov dest src)
    # dest is (rax)
    # src is 60 or (rbx)
    let destReg = a.parseReg()
    
    # Check src
    # Peek next token?
    # We need a way to peek or just parse and decide.
    # Since nifreader advances, we have to consume.
    
    # We can use savePos and restore if needed, but let's try to determine by token type.
    let pos = a.r.savePos()
    let t = next(a.r)
    if t.tk == IntLit:
      # mov reg, imm
      let imm = parseBiggestInt($t.s)
      # check if 32 or 64
      if imm >= low(int32) and imm <= high(int32):
         a.buf.emitMovImmToReg32(destReg, int32(imm))
      else:
         a.buf.emitMovImmToReg(destReg, imm)
      
      let endT = next(a.r)
      assert endT.tk == ParRi
    elif t.tk == ParLe:
      # Could be (reg) or (mem ...) or tag
      let name = $t.s
      if name in a.regMap:
        # (reg)
        let srcReg = a.regMap[name]
        let endReg = next(a.r)
        assert endReg.tk == ParRi
        a.buf.emitMov(destReg, srcReg)
        let endT = next(a.r)
        assert endT.tk == ParRi
      else:
        # (mem ...) or (+ 56) or similar?
        # For now, assume register or int
        quit "Unsupported source for mov: " & name
    else:
      quit "Unexpected token in mov source"

  of LabTagId:
    # (lab :name)
    let t = next(a.r)
    if t.tk == SymbolDef:
      let name = $t.s
      let labelId = a.getLabel(name)
      a.buf.defineLabel(labelId)
    else:
      quit "Expected SymbolDef for label (e.g. :name), got " & $t.tk
    let endT = next(a.r)
    assert endT.tk == ParRi

  of CallTagId:
    # (call label)
    let t = next(a.r)
    if t.tk == Symbol:
      let name = $t.s
      let labelId = a.getLabel(name)
      a.buf.emitCall(labelId)
    else:
      quit "Expected Symbol for call (e.g. name), got " & $t.tk
    # Consume args...
    while true:
      let subT = next(a.r)
      if subT.tk == ParRi: break
      if subT.tk == ParLe:
        let subTag = a.parseTag(subT)
        a.compileStmt(subTag)

  of RodataTagId:
    # (rodata :name "string")
    let tName = next(a.r)
    if tName.tk == SymbolDef:
      let name = $tName.s
      let labelId = a.getLabel(name)
      a.buf.defineLabel(labelId)
    else:
      quit "Expected SymbolDef for rodata name (e.g. :name), got " & $tName.tk
      
    let tStr = next(a.r)
    if tStr.tk == StringLit:
      let s = decodeStr(tStr)
      for c in s:
        a.buf.add(byte(c))
    else:
      quit "Expected string literal for rodata"
      
    let endT = next(a.r)
    assert endT.tk == ParRi

  of LeaTagId:
    # (lea (dest) src)
    let destReg = a.parseReg()
    
    let t = next(a.r)
    if t.tk == Symbol:
        # (lea (reg) symbol) -> LEA reg, [RIP + symbol]
        let name = $t.s
        let labelId = a.getLabel(name)
        a.buf.emitLea(destReg, labelId)
        let endT = next(a.r)
        assert endT.tk == ParRi
    elif t.tk == ParLe:
        let subTag = a.parseTag(t)
        # Potentially handle (mem ...) here later
        quit "Complex lea expressions not supported yet, use symbol for RIP-relative addressing"
    else:
        quit "Expected symbol or expression in lea"

  # Add other instructions as needed
  else:
    # Skip unknown tags recursively
    # echo "Skipping tag: ", tag
    while true:
      let t = next(a.r)
      if t.tk == ParRi: break
      if t.tk == ParLe:
          # consume nested
          var depth = 1
          while depth > 0:
              let n = next(a.r)
              if n.tk == ParLe: inc depth
              elif n.tk == ParRi: dec depth

proc writeElf(a: var Assembler) =
  a.buf.finalize()
  let code = a.buf.data
  
  # Entry point: assume 0x400000 + headers + code offset
  # Simplified: Load at 0x400000
  
  let baseAddr = 0x400000.uint64
  let headersSize = 64 + 56 # Ehdr + Phdr
  let entryAddr = baseAddr + headersSize.uint64 # Entry at start of code
  
  var ehdr = initHeader(entryAddr)
  let fileSize = headersSize + code.len
  let memSize = fileSize
  var phdr = initPhdr(0, baseAddr, fileSize.uint64, memSize.uint64, PF_R or PF_X)
  
  var f = newFileStream(a.outfile, fmWrite)
  defer: f.close()
  
  f.write(ehdr)
  f.write(phdr)
  if code.len > 0:
    f.writeData(unsafeAddr code[0], code.len)
  
  # Make executable
  let perms = {fpUserExec, fpGroupExec, fpOthersExec, fpUserRead, fpUserWrite}
  setFilePermissions(a.outfile, perms)

proc handleCmdLine() =
  var filename = ""
  var outfile = ""
  
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if filename.len == 0: filename = key
      else: quit "Error: multiple input files not supported yet"
    of cmdLongOption, cmdShortOption:
      case key.normalize
      of "output", "o": outfile = val
      of "help", "h": quit(Usage, QuitSuccess)
      of "version", "v": quit(Version, QuitSuccess)
      else: quit "Error: unknown option: " & key
    of cmdEnd: assert false

  if filename.len == 0:
    quit(Usage, QuitSuccess)
  
  if outfile.len == 0:
    outfile = filename.changeFileExt("") # No extension for executable

  var a = initAssembler(filename, outfile)
  try:
    process(a)
    writeElf(a)
  finally:
    close(a)

when isMainModule:
  handleCmdLine()
