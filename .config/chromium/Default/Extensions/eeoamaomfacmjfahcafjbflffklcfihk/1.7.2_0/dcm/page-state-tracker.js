(function() {
    function PageStateTracker(doc) {
        var port = chrome.extension.connect({name: "page-state-tracker"});
        doc.addEventListener("webkitvisibilitychange", onVisibilityChange, false);
        doc.addEventListener('contextmenu', onContextMenu, false);

        function onVisibilityChange() {
            port.postMessage({id: 1, data: doc.webkitVisibilityState});
        }

        function onContextMenu(event) {
            var element = getElement(event.target, "A");
            if (element && element.hasAttribute("href")) {
                var href = element.getAttribute("href");
                var url = toAbsoluteURL(href);
                port.postMessage({id: 2, data: url});
            }
        }

        function getElement(element, tagName) {
            if (element && element.tagName) {
                if (element.tagName.toUpperCase() == tagName) {
                    return element;
                } else {
                    return getElement(element.parentNode, tagName);
                }
            }

            return null;
        }

        function toAbsoluteURL (url) {
            // Handle absolute URLs (with protocol-relative prefix)
            // Example: //domain.com/file.png
            if (url.search(/^\/\//) != -1) {
                return window.location.protocol + url;
            }

            // Handle absolute URLs (with explicit origin)
            // Example: http://domain.com/file.png
            if (url.search(/:\/\//) != -1) {
                return url;
            }

            // Handle absolute URLs (without explicit origin)
            // Example: /file.png
            if (url.search(/^\//) != -1) {
                return window.location.origin + url;
            }

            // Handle relative URLs
            // Example: file.png
            var base = window.location.href.match(/(.*\/)/)[0];
            return base + url;
        }

        onVisibilityChange();
    }

    new PageStateTracker(document);
})();
