var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var DarkReader;
(function (DarkReader) {
    var Popup;
    (function (Popup) {
        /**
         * Control for selecting a font.
         */
        var FontSelect = (function (_super) {
            __extends(FontSelect, _super);
            function FontSelect(markup) {
                var _this = this;
                _super.call(this, markup, [
                    new xp.HBox({ style: 'fontSelectLine' }, [
                        new xp.TextBox({
                            style: 'fontSelectTextBox',
                            flex: 'stretch',
                            init: function (tb) { return _this.textBox = tb; },
                            notifyOnKeyDown: true,
                            onTextChange: function (e) { return _this.scrollToItemByText(e.newText); }
                        }),
                        new xp.Button({
                            style: 'fontSelectTick iconButton',
                            flex: 'none',
                            icon: '.down',
                            init: function (b) { return _this.tick = b; },
                            onClick: function (e) {
                                if (_this.isListExpanded) {
                                    _this.collapseList();
                                }
                                else {
                                    _this.scrollToItemByText(_this.textBox.text);
                                }
                            }
                        })
                    ]),
                    new xp.VBox({
                        style: 'fontList collapsed',
                        flex: 'stretch',
                        init: function (v) { return _this.fontList = v; }
                    })
                ]);
                this.onClick.addHandler(function (e) { return e.domEvent.stopPropagation(); });
                this.outerClickHandler = function () { return _this.collapseList(); };
            }
            FontSelect.prototype.getTemplate = function () {
                var t = _super.prototype.getTemplate.call(this);
                t.classList.add('FontSelect');
                return t;
            };
            FontSelect.prototype.setDefaults = function () {
                _super.prototype.setDefaults.call(this);
                this.fonts = [];
            };
            FontSelect.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.defineProperty('fonts', {
                    setter: function (fonts) {
                        _this.areFontsRendered = false;
                    }
                });
                this.defineProperty('selectedFont', {
                    setter: function (font) {
                        _this.textBox.text = font;
                    }
                });
            };
            Object.defineProperty(FontSelect.prototype, "isListExpanded", {
                // Expand/collapse font list
                get: function () {
                    return !this.fontList.domElement.classList.contains('collapsed');
                },
                enumerable: true,
                configurable: true
            });
            FontSelect.prototype.expandList = function (done) {
                var _this = this;
                var expand = function () {
                    _this.fontList.domElement.classList.remove('collapsed');
                    window.addEventListener('click', _this.outerClickHandler);
                    done && done();
                };
                if (!this.areFontsRendered) {
                    this.renderFonts(this.fonts, expand);
                }
                else {
                    expand();
                }
            };
            FontSelect.prototype.collapseList = function () {
                this.fontList.domElement.classList.add('collapsed');
                window.removeEventListener('click', this.outerClickHandler);
            };
            //
            // --- Event handlers ---
            FontSelect.prototype.onPickFont = function (f) {
                this.collapseList();
                this.onInput('selectedFont', f);
            };
            FontSelect.prototype.scrollToItemByText = function (text) {
                var _this = this;
                var onListExpanded = function () {
                    text = text.toLowerCase().trim();
                    for (var i = 0; i < _this.fonts.length; i++) {
                        if (_this.fonts[i].toLowerCase().indexOf(text) === 0) {
                            // Scroll to item
                            var item = _this.fontList.children.filter(function (c) { return c.text === _this.fonts[i]; })[0];
                            if (item) {
                                item.domElement.scrollIntoView(true);
                            }
                            break;
                        }
                    }
                };
                if (!this.isListExpanded) {
                    this.expandList(onListExpanded);
                }
                else {
                    onListExpanded();
                }
            };
            FontSelect.prototype.renderFonts = function (fonts, done) {
                var _this = this;
                // Add new font items
                this.domElement.classList.add('renderingFonts');
                this.fontList.removeChildren();
                if (fonts) {
                    // WARNING: Slow fonts rendering (100 fonts >1000ms).
                    setTimeout(function () {
                        fonts = fonts.slice(0);
                        console.time('Rendering fonts');
                        for (var i = 0; i < fonts.length; i++) {
                            (function (font) {
                                _this.fontList.append(new xp.Label({
                                    style: 'fontItem',
                                    text: font,
                                    onClick: function () { return _this.onPickFont(font); },
                                    init: function (el) { return el.domElement.style.fontFamily = font; }
                                }));
                            })(fonts[i]);
                        }
                        setTimeout(function () {
                            console.timeEnd('Rendering fonts');
                            _this.domElement.classList.remove('renderingFonts');
                            _this.areFontsRendered = true;
                            done && done();
                        }, 0);
                    }, 100); // Timeout needed as far as "Loading" message is not shown.
                }
            };
            return FontSelect;
        })(xp.VBox);
        Popup.FontSelect = FontSelect;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
