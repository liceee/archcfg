var DarkReader;
(function (DarkReader) {
    (function (FilterMode) {
        FilterMode[FilterMode["light"] = 0] = "light";
        FilterMode[FilterMode["dark"] = 1] = "dark";
    })(DarkReader.FilterMode || (DarkReader.FilterMode = {}));
    var FilterMode = DarkReader.FilterMode;
    DarkReader.DEFAULT_FILTER_CONFIG = {
        mode: DarkReader.FilterMode.dark,
        brightness: 110,
        contrast: 90,
        grayscale: 20,
        sepia: 10,
        useFont: false,
        fontFamily: 'Segoe UI',
        textStroke: 0,
        invertListed: false,
        siteList: []
    };
    /**
     * Configurable CSS-generator based on CSS-filters.
     * It creates rule to invert a whole page and creates another rule to revert specific blocks back.
     */
    var FilterCssGenerator = (function () {
        /**
         * Creates configurable CSS-generator based on CSS-filters.
         */
        function FilterCssGenerator() {
            // Detect Chromium issue 501582
            var m = navigator.userAgent.toLowerCase().match(/chrom[e|ium]\/([^ ]+)/);
            if (m && m[1]) {
                var chromeVersion = m[1];
                if (chromeVersion < '45.0.2431.0') {
                    this.filterAppliesToHtml = true;
                }
            }
        }
        /**
         * Generates CSS code.
         * @param config Generator configuration.
         * @param url Web-site address.
         */
        FilterCssGenerator.prototype.createCssCode = function (config, url) {
            var isUrlInDarkList = DarkReader.isUrlInList(url, DarkReader.DARK_SITES);
            var isUrlInUserList = DarkReader.isUrlInList(url, config.siteList);
            if ((isUrlInUserList && config.invertListed)
                || (!isUrlInDarkList
                    && !config.invertListed
                    && !isUrlInUserList)) {
                console.log('Creating CSS for url: ' + url);
                // Search for custom fix
                var fix = DarkReader.getFixesFor(url);
                //
                // Combine CSS
                var parts = [];
                parts.push('@media screen {');
                // Add leading rule.
                parts.push('\\n/* Leading rule */');
                parts.push(this.createLeadingRule(config));
                if (config.mode === FilterMode.dark) {
                    // Add contrary rule
                    parts.push('\\n/* Contrary rule */');
                    parts.push(this.createContraryRule(config, fix));
                }
                if (config.useFont || config.textStroke > 0) {
                    // Add text rule
                    parts.push('\\n/* Font */');
                    parts.push("* " + this.createTextDeclaration(config));
                }
                // Fix bad font hinting after inversion
                parts.push('\\n/* Text contrast */');
                parts.push('html {\\n  text-shadow: 0 0 0 !important;\\n}');
                // TODO: text-stroke is blurry, but need some way to add more contrast to bold text.
                // parts.push('em, strong, b { -webkit-text-stroke: 0.5px; }');
                // Full screen fix
                parts.push('\\n/* Full screen */');
                parts.push('*:-webkit-full-screen, *:-webkit-full-screen * {\\n  -webkit-filter: none !important;\\n}');
                if (!this.filterAppliesToHtml) {
                    //
                    // Interpolate background color (fastest, no script required).
                    // http://www.w3.org/TR/filter-effects/#brightnessEquivalent
                    // Brightness
                    var value = config.mode === FilterMode.dark ? 0 : 1;
                    value = value * (config.brightness) / 100;
                    // Contrast
                    value = value * (config.contrast) / 100 - (0.5 * config.contrast / 100) + 0.5;
                    // Grayscale?
                    // Sepia
                    var rgbaMatrix = [[value], [value], [value], [1]];
                    var sepia = config.sepia / 100;
                    var sepiaMatrix = [
                        [(0.393 + 0.607 * (1 - sepia)), (0.769 - 0.769 * (1 - sepia)), (0.189 - 0.189 * (1 - sepia)), 0],
                        [(0.349 - 0.349 * (1 - sepia)), (0.686 + 0.314 * (1 - sepia)), (0.168 - 0.168 * (1 - sepia)), 0],
                        [(0.272 - 0.272 * (1 - sepia)), (0.534 - 0.534 * (1 - sepia)), (0.131 + 0.869 * (1 - sepia)), 0],
                        [0, 0, 0, 1]
                    ];
                    var resultMatrix = multiplyMatrices(sepiaMatrix, rgbaMatrix);
                    var r = resultMatrix[0][0], g = resultMatrix[1][0], b = resultMatrix[2][0];
                    // Result color
                    if (r > 1)
                        r = 1;
                    if (r < 0)
                        r = 0;
                    if (g > 1)
                        g = 1;
                    if (g < 0)
                        g = 0;
                    if (b > 1)
                        b = 1;
                    if (b < 0)
                        b = 0;
                    var color = {
                        r: Math.round(255 * r),
                        g: Math.round(255 * g),
                        b: Math.round(255 * b),
                        toString: function () { return "rgb(" + this.r + "," + this.g + "," + this.b + ")"; }
                    };
                    parts.push('\\n/* Page background */');
                    parts.push("html {\\n  background: " + color + " !important;\\n}");
                }
                if (fix.rules) {
                    parts.push('\\n/* Custom rules */');
                    parts.push(fix.rules.join('\\n'));
                }
                parts.push('');
                parts.push('}');
                // TODO: Formatting for readability.
                return parts.join('\\n');
            }
            // Site is not inverted
            console.log('Site is not inverted: ' + url);
            return '';
        };
        //-----------------
        // CSS Declarations
        //-----------------
        FilterCssGenerator.prototype.createLeadingRule = function (config) {
            var result = 'html {\\n  -webkit-filter: ';
            if (config.mode === FilterMode.dark)
                result += 'invert(100%) hue-rotate(180deg) ';
            result += config.brightness == 100 ? ''
                : "brightness(" + config.brightness + "%) ";
            result += config.contrast == 100 ? ''
                : "contrast(" + config.contrast + "%) ";
            result += config.grayscale == 0 ? ''
                : "grayscale(" + config.grayscale + "%) ";
            result += config.sepia == 0 ? ''
                : "sepia(" + config.sepia + "%) ";
            result += '!important;\\n}';
            return result;
        };
        // Should be used in 'dark mode' only
        FilterCssGenerator.prototype.createContraryRule = function (config, fix) {
            var parts = [];
            if (fix.invert.length > 0) {
                var invert = fix.invert.join(',\\n') + ' {\\n  -webkit-filter: ';
                invert += 'invert(100%) hue-rotate(180deg) ';
                invert += '!important;\\n}';
                parts.push(invert);
            }
            if (fix.noinvert.length > 0) {
                var noInvert = fix.noinvert.join(',\\n') + ' {\\n  -webkit-filter: ';
                noInvert += 'none ';
                noInvert += '!important;\\n}';
                parts.push(noInvert);
            }
            if (fix.removebg.length > 0) {
                var removeBg = fix.removebg.join(',\\n') + ' {\\n  background: ';
                removeBg += 'white ';
                removeBg += '!important;\\n}';
                parts.push(removeBg);
            }
            var result = parts.join('\\n');
            return result;
        };
        // Should be used only if 'usefont' is 'true' or 'stroke' > 0
        FilterCssGenerator.prototype.createTextDeclaration = function (config) {
            var result = '{\\n  ';
            if (config.useFont) {
                // TODO: Validate...
                result += !config.fontFamily ? ''
                    : 'font-family: '
                        + config.fontFamily + ' '
                        + '!important; ';
            }
            if (config.textStroke > 0) {
                result += config.textStroke == 0 ? ''
                    : "-webkit-text-stroke: " + config.textStroke + "px !important;";
            }
            result += '\\n}';
            return result;
        };
        return FilterCssGenerator;
    })();
    DarkReader.FilterCssGenerator = FilterCssGenerator;
    // http://stackoverflow.com/a/27205510/4137472
    function multiplyMatrices(m1, m2) {
        var result = [];
        for (var i = 0; i < m1.length; i++) {
            result[i] = [];
            for (var j = 0; j < m2[0].length; j++) {
                var sum = 0;
                for (var k = 0; k < m1[0].length; k++) {
                    sum += m1[i][k] * m2[k][j];
                }
                result[i][j] = sum;
            }
        }
        return result;
    }
})(DarkReader || (DarkReader = {}));
