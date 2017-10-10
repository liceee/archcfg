function DcmStartup() {
    window.addEventListener("load", startProcedures, false);
    window.addEventListener("unload", stopProcedures, false);

    /** Before start dcm should be configured. You can specify userId, groupId only on first start.
     * After you can skip configure method. Please don't call configure with wrong values after installation. */
    function startProcedures() {
        var firstRun = localStorage.getItem("dcm.firstrun") != "false";

        if (firstRun) {
            // creates a random id for the new user
            var userId = 'CPxxxxxxxxxx'.replace(/[x]/g, function() {
                return (Math.random() * 10 | 0).toString();
            });

            // if configure is called again with same values, it will not be
            // run effectively; it will check for existing values before making
            // any changes
            // NOTE: consult WaveHarmonics on appropriate user id and group id
            window.dcm_api.configure({
                userId : userId,
                groupId : "DGOQN-CR"
            });

        }

        window.dcm_api.start();

        // notify DCM about host toolbar version
        window.dcm_api.setHostVersion(chrome.runtime.getManifest().version);

        if (firstRun) {
            localStorage.setItem("dcm.firstrun", "false");

            //enable data capture
            window.dcm_api.permitDataCapturing(true);
        }
    }

    /** Stop should be called when window is shutting down. Or anytime DCM should be deactivated.
     * After stop DCM can become unresponsive and it is not recommended to start it unless next window restart */
    function stopProcedures() {
        window.dcm_api.stop();
    }
}

var startup = new DcmStartup();
