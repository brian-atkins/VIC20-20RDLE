xa -l toc -o ./vic/20rdle.bin ./src/20rdle.asm
powershell ./prependaddress.ps1
copy /Y .\vic\20rdle.prg ..\20rdle.prg
