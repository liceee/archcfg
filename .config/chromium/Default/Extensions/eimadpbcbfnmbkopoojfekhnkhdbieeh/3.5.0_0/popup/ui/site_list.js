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
        // TODO: Grid cells behaviour.
        /**
         * Site list.
         */
        var SiteList = (function (_super) {
            __extends(SiteList, _super);
            function SiteList(markup) {
                _super.call(this, markup);
                // Add one extra Text box
                this.addTextBox('', void (0), true);
            }
            SiteList.prototype.getTemplate = function () {
                var t = _super.prototype.getTemplate.call(this);
                t.classList.add('SiteList');
                return t;
            };
            SiteList.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.sitesChangeRegistrar = new xp.EventRegistrar();
                this.defineProperty('sites', {
                    setter: function (sites) {
                        _this.sitesChangeRegistrar.unsubscribeAll();
                        if (!sites) {
                            return;
                        }
                        _this.sitesChangeRegistrar.subscribe(sites.onCollectionChanged, function (args) {
                            switch (args.action) {
                                case xp.CollectionChangeAction.Create:
                                    _this.addTextBox(args.newItem, args.newIndex);
                                    break;
                                case xp.CollectionChangeAction.Delete:
                                    _this.removeTextBox(args.oldIndex);
                                    break;
                                case xp.CollectionChangeAction.Replace:
                                    _this.children[args.newIndex].text = args.newItem;
                                    break;
                                default:
                                    throw new Error('Not implemented.');
                            }
                        });
                        sites.forEach(function (s, i) { return _this.addTextBox(s, i); });
                    }
                });
            };
            SiteList.prototype.addTextBox = function (site, index, isExtraTextBox) {
                var _this = this;
                var textBox = new xp.TextBox({
                    text: site,
                    style: 'siteListTextBox',
                    placeholder: 'mail.google.com, google.*/mail etc...',
                    onTextChange: function (e) {
                        var i = _this.children.indexOf(textBox);
                        var value = e.newText.trim();
                        var isValueValid = !!value.match(/^([^\.\s]+?\.?)+$/);
                        if (isExtraTextBox) {
                            if (isValueValid) {
                                // Add new site
                                _this.sites.push(value);
                                textBox.text = '';
                                _this.focus();
                            }
                        }
                        else {
                            if (isValueValid) {
                                // Replace value
                                _this.sites[i] = value;
                            }
                            else {
                                // Remove from list
                                _this.sites.splice(i, 1);
                            }
                        }
                    }
                });
                if (index === void (0)) {
                    index = this.children.length;
                }
                this.insert(textBox, index);
            };
            SiteList.prototype.removeTextBox = function (index) {
                var textBox = this.children[index];
                textBox.remove();
            };
            /**
             * Sets focus to the last extra text box.
             */
            SiteList.prototype.focus = function () {
                this.children[this.children.length - 1].focus();
                var container = this.parent.domElement;
                container.scrollTop = container.scrollHeight;
            };
            return SiteList;
        })(xp.VBox);
        Popup.SiteList = SiteList;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
