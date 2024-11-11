# Define the binary data to prepend (0x01 0x12)
$binaryData = [byte[]](1, 18)

# Define the path to the source binary file
$sourceFile = ".\vic\20rdle.bin"

# Define the output file path
$outputFile = ".\vic\20rdle.prg"

# Write the binary data first
[IO.File]::WriteAllBytes($outputFile, $binaryData)

# Append the content of the source binary file
Add-Content -Path $outputFile -Value (Get-Content -Path $sourceFile -Raw -Encoding Byte) -Encoding Byte
