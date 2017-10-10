/**
 *
 *Browser and OS detect
 *
 */

var BrowserDetect = {
	init: function () {
		this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
		this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
		this.OS = this.searchString(this.dataOS) || "an unknown OS";
	},
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
			string: navigator.userAgent,
			subString: "Mozilla",
			identity: "Netscape",
			versionSearch: "Mozilla"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
			   string: navigator.userAgent,
			   subString: "iPhone",
			   identity: "iPhone/iPod"
	    },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	]

};
BrowserDetect.init();


/**
 *
 *Custom Google Analytics
 *
 */

if (BrowserDetect.OS == 'Windows' || BrowserDetect.OS == 'Linux') {
	
	// set up analytics
	var _gaq = _gaq || [];
		_gaq.push(['_setAccount', 'UA-295754-29']);
		_gaq.push(['_trackPageview']);
		
	(function() {
		var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
		ga.src = 'https://ssl.google-analytics.com/ga.js';
		(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(ga);
	})();
	
	// track pages
	function trackPageApp() {
		
		function pushData(cate, action, label) {
			_gaq.push(['_trackEvent', cate, action, label]);
		}
		
		// trach gross user clicks
		$('#diigoAccount').click(function(e) {
			
			var target = e.target,
					action = ''; 
			
			
			if ($(target).hasClass('button')) {
			
				if (util.findP(target, '.loginByGoogle')) {
					action = 'loginByGoogleBtn';
				}
				if (util.findP(target, '.loginByDiigo')) {
					action = 'loginByDiigoBtn';
				}
				
				pushData('Settings', action, 'click');
			}
		});
		
		// track sync	setting success times
		chrome.extension.onRequest.addListener(
			function(request, sender, sendResponse) {
				if (request.name === 'loginByGoogle' 
					|| request.name === 'loginByDiigo') {
					
					pushData('Settings', request.name, 'success');
				}
			}
		);
	}
	
	function trackPageNote() {
		// currently nothing to track except page view
	}
	
	function trackPageBg() {
		// currently nothing to track except page view
	}
	
	// init track related page
	window.addEventListener('load', function() {
		console.log(location.pathname);
		switch(location.pathname) {
		case '/app.html':
			trackPageApp();
			break;
		case '/note.html':
			trackPageNote();
			break;
		case '/bg.html':
			trackPageBg();
			break;
		}
	}, false);
}