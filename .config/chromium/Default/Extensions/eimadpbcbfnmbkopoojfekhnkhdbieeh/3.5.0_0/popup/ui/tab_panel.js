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
         * Tab panel.
         */
        var TabPanel = (function (_super) {
            __extends(TabPanel, _super);
            function TabPanel(markup, tabs) {
                var _this = this;
                _super.call(this, markup, [
                    new xp.HBox({
                        style: 'tabPanelButtons',
                        init: function (el) { return _this.buttonsContainer = el; }
                    }),
                    new xp.HBox({
                        style: 'tabPanelContainer',
                        init: function (el) { return _this.tabsContainer = el; }
                    })
                ]);
                //
                // Add tabs
                this.tabs = [];
                tabs.forEach(function (t) { return _this.addTab(t); });
            }
            TabPanel.prototype.initEvents = function () {
                var _this = this;
                _super.prototype.initEvents.call(this);
                this.onTabSwitched = new xp.Event();
                this.onRemoved.addHandler(function () { return _this.onTabSwitched.removeAllHandlers(); });
            };
            TabPanel.prototype.getTemplate = function () {
                var t = _super.prototype.getTemplate.call(this);
                t.classList.add('TabPanel');
                return t;
            };
            TabPanel.prototype.addTab = function (tab) {
                var _this = this;
                // Create tab button
                var button = new xp.Button({
                    style: 'tabPanelButton',
                    onClick: function () { return _this.switchTab(tab); },
                    useParentScope: false,
                    scope: tab,
                    text: '{tabName}'
                });
                this.buttonsContainer.append(button);
                this.tabsContainer.append(tab);
                this.tabs.push(tab);
                if (this.tabs.length === 1) {
                    this.switchTab(tab);
                }
            };
            TabPanel.prototype.removeTab = function (tab) {
                var tabIndex = this.tabs.indexOf(tab);
                if (tabIndex < 0) {
                    throw new Error('Tab panel doesn\'t contain this tab.');
                }
                this.tabs.splice(tabIndex, 1);
                tab.remove();
                this.buttonsContainer.children[tabIndex].remove();
                if (tab.active && this.tabs.length > 0) {
                    this.switchTab(this.tabs[0]);
                }
            };
            TabPanel.prototype.switchTab = function (tab) {
                var tabIndex = this.tabs.indexOf(tab);
                if (tabIndex < 0) {
                    throw new Error('Tab panel doesn\'t contain this tab.');
                }
                this.buttonsContainer.children.forEach(function (b) { return b.domElement.classList.remove('active'); });
                this.buttonsContainer.children[tabIndex].domElement.classList.add('active');
                this.tabs.forEach(function (t) { return t.active = false; });
                tab.active = true;
                this.onTabSwitched.invoke(tab);
            };
            return TabPanel;
        })(xp.VBox);
        Popup.TabPanel = TabPanel;
        /**
         * Tab.
         */
        var Tab = (function (_super) {
            __extends(Tab, _super);
            function Tab(markup, children) {
                _super.call(this, markup, children);
            }
            Tab.prototype.getTemplate = function () {
                var t = _super.prototype.getTemplate.call(this);
                t.classList.add('Tab');
                return t;
            };
            Tab.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.defineProperty('tabName', {
                    observable: true
                });
                this.defineProperty('active', {
                    getter: function () { return _this.domElement.classList.contains('active'); },
                    setter: function (active) {
                        if (active) {
                            _this.domElement.classList.add('active');
                        }
                        else {
                            _this.domElement.classList.remove('active');
                        }
                    }
                });
            };
            return Tab;
        })(xp.VBox);
        Popup.Tab = Tab;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
