$file = "e:\spatialgeography\geography\knowledgegraph.html"

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

            /* Viewport meta handled via JS below */
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
    })();
    </script>
'@

$content = [System.IO.File]::ReadAllText($file)

# Remove previous injections if any
$content = $content -replace '(?s)\s*<style id="mobile-responsive-override">.*?</style>', ''
$content = $content -replace '(?s)\s*<script id="mobile-responsive-script">.*?</script>', ''

# Inject CSS before </head>
$content = $content -replace '</head>', "$css`n</head>"

# Inject JS before </body>
$content = $content -replace '</body>', "$js`n</body>"

[System.IO.File]::WriteAllText($file, $content)

Write-Host "Done - mobile responsive styles injected"
