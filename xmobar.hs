Config
  { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
  , bgColor = "black"
  , fgColor = "grey"
  , position = TopW L 100
  , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
               , Run Memory ["-t","Mem: <usedratio>%"] 10
               , Run Swap [] 10
               , Run Network "wlo1" [] 10
               , Run Date "%a %b %_d %k:%M" "date" 10
               , Run MPD ["-t", "<state>: <artist> - <track>"] 10
               , Run StdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ %cpu% | %memory% * %swap% %wlo1% | %mpd% | <fc=#ee9a00>%date%</fc>"
  }
