$folders = @(
    "agricultural-geography", "cartography", "climatology", "economic-geography", 
    "environmental", "field-survey", "geographical-thought", "geography-of-india", 
    "geography-of-odisha", "geomorphology", "geospatial-ai", "gis-remote-sensing", 
    "human-development", "human-geography", "indian-physical", "indian-socioeco", 
    "industrial-geography", "natural-hazards", "natural-resource-management", 
    "oceanography", "physical-geography", "political-geography", "population-geography", 
    "pyq-assistant-professor", "pyq-cuet-pg", "pyq-pgt", "pyq-ugc-net", "pyq-wbset", 
    "regional-development", "research-methodology", "settlement-geography", 
    "social-and-cultural", "statistical-geography", "trade-and-transport", "urban-geography"
)

$basePath = "e:\spatialgeography\geography"
$count = 0

foreach ($folder in $folders) {
    $folderPath = Join-Path $basePath $folder
    if (Test-Path $folderPath) {
        $files = Get-ChildItem -Path $folderPath -Filter "*.md" -Recurse
        foreach ($file in $files) {
            $content = Get-Content $file.FullName
            $modified = $false
            
            $titleFixed = $false
            $headingFixed = $false

            for ($i = 0; $i -lt $content.Count; $i++) {
                $line = $content[$i]
                
                # Check for YAML title
                if (-not $titleFixed -and $line -match '^title:\s*(.+)$') {
                    $titleVal = $matches[1]
                    $quote = ""
                    if ($titleVal -match "^(`"|')(.*)(`"|')$") {
                        $quote = $matches[1]
                        $titleText = $matches[2]
                    } else {
                        $titleText = $titleVal
                    }
                    
                    $words = ($titleText -split '\s+') | Where-Object { $_ -ne "" }
                    if ($words.Count -gt 6) {
                        $shortTitle = ($words[0..5] -join " ")
                        # Remove trailing comma or hyphen if exists
                        $shortTitle = $shortTitle -replace '[,:-]$', ''
                        
                        if ($quote) {
                            $newLine = "title: ${quote}${shortTitle}${quote}"
                        } else {
                            $newLine = "title: `"${shortTitle}`""
                        }
                        $content[$i] = $newLine
                        $modified = $true
                        Write-Host "Truncated title in $($file.Name): '$titleText' -> '$shortTitle'"
                    }
                    $titleFixed = $true
                }
                
                # Check for ANY heading (usually the title of the document)
                if (-not $headingFixed -and $line -match '^(#+)\s+(.+)$') {
                    $hashes = $matches[1]
                    $headingText = $matches[2]
                    $words = ($headingText -split '\s+') | Where-Object { $_ -ne "" }
                    if ($words.Count -gt 6) {
                        $shortHeading = ($words[0..5] -join " ")
                        # Remove trailing comma or hyphen if exists
                        $shortHeading = $shortHeading -replace '[,:-]$', ''
                        
                        $content[$i] = "$hashes $shortHeading"
                        $modified = $true
                        Write-Host "Truncated heading in $($file.Name): '$headingText' -> '$shortHeading'"
                    }
                    $headingFixed = $true
                }
                
                if ($titleFixed -and $headingFixed) {
                    break
                }
            }
            
            if ($modified) {
                Set-Content -Path $file.FullName -Value $content
                $count++
            }
        }
    }
}

Write-Host "Done. Modified $count files."
