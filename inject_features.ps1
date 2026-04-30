$file = "e:\spatialgeography\geography\knowledgegraph.html"
$visitorFile = "e:\spatialgeography\geography\visitor-map.html"

$css = @'
    <style id="mobile-responsive-override">
        /* ============================================================
           MOBILE RESPONSIVE for Cosma Knowledge Graph
           ============================================================ */

        /* Tablet: shrink sidebar */
        @media screen and (max-width: 1024px) {
            aside.menu {
                flex: 0 0 22rem !important;
            }
            aside.menu footer {
                width: 22rem !important;
            }
            main.record-container {
                flex: 0 0 38rem !important;
            }
            .graph-controls {
                left: 26rem !important;
            }
            .graph-controls.move {
                left: 4rem !important;
            }
        }

        /* Mobile: full overlay panels */
        @media screen and (max-width: 768px) {
            /* Sidebar becomes a slide-in overlay */
            aside.menu {
                position: fixed !important;
                left: -100% !important;
                top: 0 !important;
                width: 85vw !important;
                max-width: 30rem !important;
                flex: none !important;
                height: 100vh !important;
                height: 100dvh !important;
                z-index: 500 !important;
                transition: left 0.3s ease !important;
                box-shadow: 2px 0 15px rgba(0,0,0,0.25) !important;
            }
            aside.menu.active {
                left: 0 !important;
                display: block !important;
            }
            aside.menu footer {
                width: 85vw !important;
                max-width: 30rem !important;
            }

            /* Menu toggle button: always visible on mobile */
            #close-left-side {
                position: fixed !important;
                top: 10px !important;
                left: 10px !important;
                z-index: 600 !important;
                background: var(--cosma-blue, #1565C0) !important;
                color: white !important;
                border: none !important;
                border-radius: 50% !important;
                width: 40px !important;
                height: 40px !important;
                font-size: 1.8rem !important;
                display: flex !important;
                align-items: center !important;
                justify-content: center !important;
                box-shadow: 0 2px 8px rgba(0,0,0,0.3) !important;
            }

            /* Graph fills full screen */
            .graph-wrapper {
                flex: 1 1 100% !important;
                width: 100% !important;
                height: 100vh !important;
                height: 100dvh !important;
            }

            /* Record panel: full-width overlay from right */
            main.record-container {
                position: fixed !important;
                top: 0 !important;
                right: 0 !important;
                width: 100vw !important;
                height: 100vh !important;
                height: 100dvh !important;
                flex: none !important;
                z-index: 400 !important;
                padding: 1.2rem 1rem !important;
            }
            main.record-container:not(.active) {
                display: none !important;
            }

            /* Record close button visible and styled */
            #close-right-side {
                position: fixed !important;
                top: 10px !important;
                right: 10px !important;
                z-index: 500 !important;
                background: var(--cosma-blue, #1565C0) !important;
                color: white !important;
                border: none !important;
                border-radius: 50% !important;
                width: 40px !important;
                height: 40px !important;
                font-size: 1.8rem !important;
                display: flex !important;
                align-items: center !important;
                justify-content: center !important;
                box-shadow: 0 2px 8px rgba(0,0,0,0.3) !important;
            }

            /* Graph controls repositioned */
            .graph-controls,
            .graph-controls.move {
                left: 10px !important;
                bottom: 12px !important;
            }

            /* Typography scaling */
            .record-title {
                font-size: 1.8rem !important;
            }
            .record main {
                font-size: 1.4rem !important;
            }
            .search-bar {
                font-size: 1.6rem !important;
                padding: 0.6rem !important;
            }
            #timeline-form {
                width: calc(100vw - 20px) !important;
            }
        }

        /* Small phones */
        @media screen and (max-width: 480px) {
            .record-title {
                font-size: 1.6rem !important;
            }
            .record main {
                font-size: 1.3rem !important;
            }
            .graph-controls button {
                font-size: 1.1rem !important;
                padding: 4px 8px !important;
            }
        }

        /* ============================================================
           INTERACTIVE GRAPH LABELS
           ============================================================ */
        
        /* Make text labels clickable and red on hover */
        #graph-canvas text,
        [data-node] text {
            pointer-events: all !important;
            cursor: pointer !important;
            transition: fill 0.2s ease !important;
        }

        #graph-canvas text:hover,
        [data-node]:hover text {
            fill: red !important;
        }

        /* Red on active/focused nodes */
        [data-node].focus text,
        [data-node].highlight text,
        g.focus text,
        g.highlight text {
            fill: red !important;
            font-weight: bold !important;
        }
    </style>
'@

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

        window.addEventListener('DOMContentLoaded', function() {
            // On mobile, hide the menu (types, keywords, etc.) by default on load
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

            // Make graph text labels clickable
            var graphCanvas = document.getElementById('graph-canvas');
            if (graphCanvas) {
                graphCanvas.addEventListener('click', function(e) {
                    if (e.target && e.target.tagName && e.target.tagName.toLowerCase() === 'text') {
                        var nodeGroup = e.target.closest('[data-node]');
                        if (nodeGroup) {
                            var nodeId = nodeGroup.getAttribute('data-node');
                            if (nodeId) {
                                // Set hash to trigger Cosma's built-in navigation
                                window.location.hash = '#' + nodeId;
                                e.stopPropagation();
                            }
                        }
                    }
                }, true);
            }
        });
    })();
    </script>
'@

# Read knowledge graph content
$content = [System.IO.File]::ReadAllText($file)

# Remove previous injections
$content = $content -replace '(?s)\s*<style id="mobile-responsive-override">.*?</style>', ''
$content = $content -replace '(?s)\s*<script id="mobile-responsive-script">.*?</script>', ''
$content = $content -replace '(?s)\s*<div id="visitor-map-injected">.*?</div>\s*', ''

# Inject CSS and JS
$content = $content -replace '</head>', "$css`n</head>"
$content = $content -replace '</body>', "$js`n</body>"

# Inject visitor map before the footer
$visitorHtml = [System.IO.File]::ReadAllText($visitorFile)
$visitorHtmlWrapped = "`n<div id=`"visitor-map-injected`">`n$visitorHtml`n</div>`n"
$content = $content -replace '<footer class="menu-footer">', "$visitorHtmlWrapped<footer class=`"menu-footer`">"

[System.IO.File]::WriteAllText($file, $content)

Write-Host "Done - styles, JS, and visitor map injected."
