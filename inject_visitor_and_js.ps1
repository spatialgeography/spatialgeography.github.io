$file = "e:\spatialgeography\geography\knowledgegraph.html"
$visitorFile = "e:\spatialgeography\geography\visitor-map.html"

# Read knowledge graph content
$content = [System.IO.File]::ReadAllText($file)

# 1. Update the mobile responsive JS
$js = @'
    <script id="mobile-responsive-script">
    (function() {
        // Ensure viewport meta tag exists for mobile
        if (!document.querySelector('meta[name="viewport"]')) {
            var meta = document.createElement('meta');
            meta.name = 'viewport';
            meta.content = 'width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no';
            document.head.appendChild(meta);
        }

        // On mobile, hide the menu (types, keywords, etc.) by default on load
        window.addEventListener('DOMContentLoaded', function() {
            if (window.innerWidth <= 768) {
                var menuContainer = document.getElementById('menu-container');
                var closeBtn = document.getElementById('close-left-side');
                if (menuContainer && menuContainer.classList.contains('active')) {
                    menuContainer.classList.remove('active');
                }
                if (closeBtn && closeBtn.classList.contains('active')) {
                    closeBtn.classList.remove('active');
                }
            }
        });
    })();
    </script>
'@

# Replace the old script
$content = $content -replace '(?s)\s*<script id="mobile-responsive-script">.*?</script>', ''
$content = $content -replace '</body>', "$js`n</body>"

# 2. Inject visitor map
$visitorHtml = [System.IO.File]::ReadAllText($visitorFile)
$visitorHtmlWrapped = "<div id=`"visitor-map-injected`">`n$visitorHtml`n</div>"

# First, remove it if it was already injected
$content = $content -replace '(?s)<div id="visitor-map-injected">.*?</div>\s*', ''

# Now inject it right before `<footer class="menu-footer">`
$content = $content -replace '<footer class="menu-footer">', "$visitorHtmlWrapped`n<footer class=`"menu-footer`">"

[System.IO.File]::WriteAllText($file, $content)

Write-Host "Done - mobile JS updated to hide menu by default, and visitor map injected."
