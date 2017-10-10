//
//if(!localStorage['menu_id'] && !localStorage['promotion']){
//	window.open("https://www.diigo.com/promotion/quick_note.html")
//	localStorage['promotion'] =1;
//}
/*==================  Main function ====*/

chrome.browserAction.onClicked.addListener(function(){
	chrome.tabs.create({url:'app.html'});
});


var tab = {},
	selection = '',
	notesUpdated = true, //flag if notes are updated in app.html
	req = new XMLHttpRequest();

var Ext = {
	getURL: function(path) {
		return chrome.extension.getURL(path);
	},
	update: function(tabId, updateProperties) {
		chrome.tabs.update(tabId, updateProperties);
	},
	onSelectionChanged: function() {
		chrome.tabs.onSelectionChanged.addListener(function(tabId, selectInfo) {
			chrome.tabs.get(tabId, function(tab) { 
				if (!notesUpdated && tab.url==Ext.getURL('app.html')) 
					Ext.update(tabId, {url:tab.url});
					notesUpdated = true;
			});
			
			chrome.contextMenus.update(util.menu(), {title: 'New Note'});
		});
	}
};	
	
var util = {
	sendRequest: function(id, req) {
		chrome.tabs.sendRequest(id, req);
	},
	error: function(e) { 
		console.error('Note error: '+e.message);
	},
	require: function(loc) {
		var script = document.createElement('script');
		script.setAttribute('type', 'text/javascript');
		script.setAttribute('src', loc);
		document.head.appendChild(script);
	},
	processImage: function(request, sendResponse) {
	  try {
		var req = new XMLHttpRequest();
		req.open('GET', request.href, false);
		if (request.binary) req.overrideMimeType('text/plain; charset=x-user-defined');
		req.send();
		if (req.readyState == 4 && req.status == 200) {
		  request.contentType = req.getResponseHeader('Content-Type');
		  request.data = req.responseText;
		}
		else {
		  request.error = "Unexpected error";
		}
	  }
	  catch (e) {
		console.error('Download error: ' + e.message);
		request.error = e.message;
	  }
	  sendResponse(request);
	},
	getUTCString: function(time) {
		if (time == 'now' || !time) {
			var d = new Date();
			d = Date.parse(d) / 1000;
			return util.getUTCString(d);
		}
		
		var y, m, d, h, M, s; 
		time = new Date(time * 1000);
		y = time.getUTCFullYear();
		m = time.getUTCMonth()+1;
		d = time.getUTCDate();
		h = time.getUTCHours();
		M = time.getUTCMinutes();
		s = time.getUTCSeconds();
		if (m < 10) m = '0' + m;
		if (d < 10) d = '0' + d;
		if (h < 10) h = '0' + h;
		if (M < 10) M = '0' + M;
		if (s < 10) s = '0' + s;
		return 	y + '-' + m + '-' + d + ' ' + h + ':' + M + ':' + s;
	},
	getUTCInt: function(time) {
		if (time == 'now' || !time) {
			time = new Date(); // local
			return Date.parse(time) / 1000;
		} else {
			time = time.split(' ');
			t0 = time[0].split('-');
			t1 = time[1].split(':');
			// month
			t0[1] -= 1; 
			if (t0[1] < 10) t0[1] = '0' + t0[1];
			return Date.UTC(t0[0], t0[1], t0[2], t1[0], t1[1], t1[2]) / 1000; 
		}
	},
	
	tabChange: function() {
		chrome.tabs.onSelectionChanged.addListener(function(tabId, selectInfo) { 
			chrome.contextMenus.update(util.menu(), {title:'New Note'});
			//chrome.tabs.onSelectionChanged.removeListener();
		});
		chrome.tabs.onUpdated.addListener(function(tabId, changeInfo, tab) { 
			chrome.contextMenus.update(util.menu(), {title:'New Note'});
			// listen for user register to diigo
			if (changeInfo.url == "https://www.diigo.com/quicknotedone" ||changeInfo.url == "https://www.diigo.com/quicknotedone#") {
                console.log('google');
				chrome.tabs.remove(tab.id);
				diigo.googleflag=1;
				diigo.init();
				chrome.extension.sendRequest({name: 'loginByGoogle'});
			}
		});
	},
	
	menu: function() {
		//console.log(localStorage['menu_id']);
		//chrome.contextMenus.remove(parseInt(localStorage['menu_id']));
		var id = chrome.contextMenus.create({
			title: 'New Note', 
			onclick: function(info, tab) {
				if (info.pageUrl.match(/https:\/\/chrome.google.com\/[extensions|webstore]/i))
					return alert('Quick Note can\'t work in Extension Gallery or Web Store.');
					
				tab = tab;
				chrome.tabs.executeScript(tab.id, {file: 'js/isload.js'}); 				// kick start note panel
				//chrome.tabs.sendRequest(tab.id, {name:'init'});
				//chrome.tabs.sendRequest(tab.id, {name:'get_selection', selection:selection});
			}, 
			contexts:['all']
		});
		//console.log(id);
		localStorage['menu_id'] = id;
		util.menu = function() { return id; };
		return id;
	}
}

// sync diigo
var diigo = {
	i: 0,
	status: '',
	flag:0,
    syncStatus:false,
	post: {
		pv	: 1,
		cv	:	'1.6.6',
		ct	: 'chrome_quick_note',
		v		:	1,
		cmd	: '',
		json: '', // encodeURIComponent string
		s		: '',	// MD5(ct+cv+json+v+cmd)
		url	: 'https://www.diigo.com/kree'
	},
    SyncMAXItemNumber:50,
    conflictCount:0,

	base64ToURL: function(desc) {
		var temp = document.createElement('div');
		temp.innerHTML = desc;
		var imgs = temp.getElementsByTagName('img');
		for (var i=0, len=imgs.length; i<len; i++) 
			// for compatability: old version(pre1.2) of QN has no dataset['src']
			imgs[i].src = imgs[i].dataset['src'] || imgs[i].src; 
		
		desc = temp.innerHTML;
		temp = imgs = null; // release memory?
		return desc;
	},

    /*Sync init*/
    SyncBegin:function(callback){
        if(this.syncStatus) return;  //return if is syning.
        this.syncStatus = true;

        //sync begin
        chrome.extension.sendRequest({name: 'show_syncing'});
        try{
            this.uploadNewItems(callback);
        }catch(e){
            diigo.SyncFinish();
        }
    },
    SyncFinish:function(){
        this.syncStatus = false;
        chrome.extension.sendRequest({name: 'sync_done',conflictCount:diigo.conflictCount});
    },
    SyncError:function(){

    },



	getItem: function(item) {
		
		function isRich(str){
			if(str.match(/<\s?(img|a|p|h1|h2|h3|h4|ul|li|dd|dt)[^>]*>/i)!= undefined) return true;
			if(str.match(/<*(style|font|color)=[^>]*>/i)!= undefined) return true;
			if(str.match('<div attr="rich"></div>')!=undefined) return true;
			return false;
		}
		function HtmlToText(str){
			str = encodeURI(str);
			str = str.replace(/%0A/gi,'');
			str = decodeURI(str);
			str=str.replace(/<div><br*>/gi,'\r\n');
			str=str.replace(/<div>/gi,'\r\n');
			str=str.replace(/<\/div>/gi,'');
			str=str.replace(/<br*>/gi,'\r\n');
			str=str.replace(/<br \/>/gi,'\r\n');
			str=str.replace(/<\s?(span|code|\/span|\/code|cite|\/cite)[^>]*>/gi,'');
			return str;
		}
		function deleterichmark(str){
			str = str.replace(/<div attr=\"rich\"><\/div>/g,'');
			return str;
		}
		
		if (item.title == undefined) {	// sync item
			return {
				local_id: item.id,
				server_id: item.server_id,
				type: 1,
				mode: item.mode
			};
		}
		else if (item.list == 'trash' && item.server_id == -1) { // not sync and trashed item
			return null;
		} 
		else if (item.list == 'trash' && item.server_id != -1) { // delete(4) item
			return {
				local_id: item.id,
				server_id: item.server_id,
//				folder_server_id: 0, // all notes
				cmd: 4,
				type: 1	// text note
			};
		}
		else {													// new(1) or update(2) item
			if (!item.title && !item.desc && item.server_id == -1) {
				return null; // empty and not synced note
			}
			else {
				console.log(item.mode);
				return {
					local_id: item.id,
					server_id: item.server_id,
//					folder_server_id: 0,
					cmd: item.server_id==-1 ? 1 : 2,
					type: /*1*/ isRich(item.desc)?4:1,	
					title: item.title,
					content: /* (!item.title && !item.desc) ? 'new note' :  */diigo.base64ToURL(isRich(item.desc)?deleterichmark(item.desc):HtmlToText(item.desc)),
					local_created_at: util.getUTCInt(item.created),
					local_updated_at: util.getUTCInt(item.updated),
					mode: (item.mode=='undefined')?2:item.mode,
                    tags: (item.tag=='undefined')?"":item.tag
				};
			}
		}
	},
	getItems: function(rows) {
		var items = [];
		for (var i=0, len=rows.length; i<len; i++) {
			var item = diigo.getItem(rows.item(i));
			if (item) {
				items.push(item);
			}
		}
		return items;
	},
	
	// used by loadItems
	serverNew: function(notes, user_id, callback) {
		var i = 0, len = notes.length, note;
		
		function store() {
			note = notes[i];
			db.tx(
				{	name: 'transaction', 
					query: 'SELECT id FROM diigo WHERE server_id=?', // check if item stored
					row: [note.server_id]
				}, 
				function(tx, ds) {
					if (ds.rows.length) {
						tx.executeSql('UPDATE diigo SET mode=? WHERE local_id=?',
							[note.mode, note.local_id]);
						return storeNext();
					}
					var thisSyncHash = hex_md5(note.title + note.content);
					if(note.type==1){
						//format cleantext to html
						note.content = note.content.replace(/\r/g,'');
						note.content = note.content.replace(/\n/g,'<br />'); 
					}else if(note.type==4){
						//for mark rich text
						note.content = '<div attr="rich"></div>'+note.content;
					}
					
					tx.executeSql(
						'INSERT INTO notes (title, desc, list, tag, updated, created,lastSyncHash) VALUES (?, ?, ?, ?, ?, ?, ?);',
						[	note.title, note.content, '', note.tags, 
							util.getUTCString(note.server_updated_at),
							//util.getUTCString('now'), //use local time as notes updated
							util.getUTCString(note.server_created_at),thisSyncHash	],
						 
						function(tx, ds) {
							tx.executeSql('UPDATE diigo SET server_id=?, user_id=?, mode=?, sync_flag=1 WHERE local_id=?',
								[note.server_id, user_id, note.mode, ds.insertId]);
							/* tx.executeSql(
								'INSERT INTO diigo (local_id, server_id, server_created_at, server_updated_at) VALUES (?, ?, ?, ?)',
								[	ds.insertId, note.server_id, 
									util.getUTCString(note.server_created_at), 
									util.getUTCString(note.server_updated_at)]
							); */
							storeNext();
						}
					);
				}
			);
		}
		
		function storeNext() {
			if (++i < len) store();
			else callback();
		}
		
		store();
	},
	// util.request('signin', {user: 'imiaou@gmail.com', password: 'imiaoumo'}, function() {})
	request: function(cmd, json, callback) {
		var post 	= diigo.post;
		post.json = JSON.stringify(json);

		var that = this;

		
        if(cmd=='signin' || diigo.googleflag==1){

            var postdata = {
                cv:post.cv,
                ct:post.ct,
                v:post.v,
                cmd:cmd,
                json:post.json,
                s:hex_md5(''+post.ct+post.cv+post.json+post.v+cmd)
            }
        }else{
            var username = JSON.parse(localStorage['diigo']).user;

            var postdata = {
                cv:post.cv,
                ct:post.ct,
                v:post.v,
                cmd:cmd,
                json:post.json,
                uname:username,
                s:hex_md5(''+post.ct+post.cv+post.json+post.v+cmd+username)
            }
        }


        $.ajax({
            type:"POST",
            url:post.url + ('/pv=' + post.pv + '/ct=' + post.ct),
            data:postdata,
            success:function(responseText){
                if (cmd == 'signin') {
                    chrome.extension.sendRequest({name: 'loginByDiigo'});
                }
                diigo.response(JSON.parse(responseText),post.json,cmd,callback);
            },
            statusCode:{401:function(res){
                if(JSON.parse(res.responseText).code == -1){
                    chrome.extension.sendRequest({name: 'auth_failure'});
                }
                that.syncStatus = false;
            }},
            error:function(error){
                if(error.status!=401){
                    that.syncStatus =false;
                    chrome.extension.sendRequest({name: 'network_error', status: req.status});
                }
            }

        });



	},
	response: function(res, req,cmd, callback) {
		// console.log(res,req);
		
		// if (localStorage['diigo'] == '1') {
		if(cmd=='signin' || diigo.googleflag==1){
			// if user has signed in with the same account
			// restore his/her previous info.
			diigo.googleflag=0;
			var oldUserInfo = localStorage['diigo'+res.user_id];
			if (oldUserInfo) {
				oldUserInfo = JSON.parse(oldUserInfo);
				localStorage['diigo_load_stamp'] = oldUserInfo.load_stamp;
				localStorage['diigo_upload_stamp'] = oldUserInfo.upload_stamp;
			}	
			if(res.result.permission){
				var is_premium = res.result.permission.is_premium;
			}else{
				var is_premium = false;
			}
			localStorage['diigo'] = JSON.stringify({
				user: res.user,
				user_id: res.user_id,
				is_premium:is_premium
			});
			if(cmd=='signin'){
				diigo.init();
			}
			chrome.extension.sendRequest({name: 'after_auth'});
		}
		// newNotes: new notes from server or other clients
		var /* notes = items =  */newNotes = [], note = {};
		var notes = res.result.items || res.result.folder_server_id_0 || [];
		var items = req.items || req.folder_server_id_0 || [];
		/* if (res.result.folder_server_id_0) {
			notes = res.result.folder_server_id_0;
			items = req.folder_server_id_0;
		} 
		else if (res.result.items) {
			notes = res.result.items;
			items = req.items || []; // [] for loadItems
		}
		else {
			return;
		} */
		//diigo.URLToBase64(notes, items);

        var SyncItemFlag = 0;
        function callbackForSyncStep(){
            if(diigo.status == "syncItems"){
                SyncItemFlag++;
                if(SyncItemFlag == notes.length){
                    callback(res);
                }
            }
        }

		for (var i = 0, len = notes.length; i<len; i++) {
			note = notes[i];
			
			switch (note.cmd) {
			case 1:
				if (diigo.status == 'uploadNew') {
					// created by client  uploadNewItems
					db.tx({name: 'transaction', 
						query: 'UPDATE diigo SET server_id=?, server_created_at=?, '
										+ 'server_updated_at=?, user_id=?, mode=?,sync_flag=1 WHERE local_id=?',
						row: [note.server_id, util.getUTCString(note.server_created_at), 
									util.getUTCString(note.server_updated_at), res.user_id, 
									note.mode, note.local_id]
					}, function(){});
                    db.tx({name: 'transaction',
                        query:"UPDATE notes SET lastSyncHash = ? WHERE id = ?",
                        row: [hex_md5(note.title+note.content),note.local_id]
                    },function(){});
				} 
				else {
					// created by server or other clients
					newNotes.push(note);
				}
				break;
			case 2:
                if(diigo.status == "syncItems"){
                    //response note from sync items (sync step 2)
                    (function(note,i,len){
                        db.tx({
                            name:"transaction",
                            query:"SELECT * FROM notes,diigo WHERE notes.id = ? AND notes.id = diigo.local_id",
                            row:[note.local_id]
                        },function(tx,ds){
                            var row = ds.rows.item(0)
                            if(row.sync_flag == 1){
                                //sync case 2, only modify with server.
                                note.name = "update_item";
                                note.lastSyncHash = hex_md5(note.title + note.content);
                                if(note.type == 1){
                                    note.content = note.content.replace(/\r/g,'').replace(/\n/g,'<br />');
                                }else if(note.type==4){
                                    note.content = '<div attr="rich"></div>'+note.content;
                                }
                                db.tx(note);
                                db.tx({name: 'transaction',
                                    query: 'UPDATE diigo SET mode=?,server_updated_at=?,sync_flag=1 WHERE local_id=?',
                                    row: [note.mode,util.getUTCString(note.server_created_at), note.local_id]
                                }, function(){
                                    callbackForSyncStep();
                                });
                            }else{

                                var thisSyncHash = hex_md5(note.title + note.content);
                                if(thisSyncHash == row.lastSyncHash){
                                    //sync case 1
                                    //this is mean only modify with client , skip it and will upload the update
                                    //in sync step 3 (upload update);
                                    callbackForSyncStep();
                                }else{
                                    console.log("conflict");
                                    diigo.conflictCount++;
                                    //sync case 3
                                    //modify both server and client, copy local note and move to conflict list,
                                    //then update the note with server response.

                                    // move current note into conflict list and set the sync_flag = -2;
                                    db.tx({
                                        name:"transaction",
                                        query:"UPDATE notes SET list=? WHERE id=?",
                                        row:["conflict",note.local_id]
                                    },function(tx,ds){});
                                    db.tx({
                                        name:"transaction",
                                        query:'UPDATE diigo SET sync_flag=-2 WHERE local_id = ?',
                                        row:[note.local_id]
                                    },function(){
                                            callbackForSyncStep();
                                    });

                                    //create a new note with server response.
                                    if(note.type == 1){
                                        note.content = note.content.replace(/\r/g,'').replace(/\n/g,'<br />');
                                    }else if(note.type==4){
                                        note.content = '<div attr="rich"></div>'+note.content;
                                    }

                                    db.tx({
                                        name:"transaction",
                                        query:"INSERT INTO notes (title, desc, list, tag, updated, created,lastSyncHash) VALUES (?, ?, ?, ?, ?, ?,?);",
                                        row:[note.title, note.content, row.list, row.tag , row.updated, row.created, thisSyncHash]
                                    },function(tx,ds){
                                        tx.executeSql('UPDATE diigo SET server_id=?, user_id=?, mode=?, server_updated_at=?,sync_flag=1 WHERE local_id=?',
                                            [note.server_id,res.user_id,note.mode,util.getUTCString(note.server_created_at),ds.insertId]);
                                    });

                                }

                            }
                        });

                    })(note,i,len);

                }else{
                    //response note from upload update item (sync step 3)
                    db.tx({
                        name:"transaction",
                        query: 'UPDATE diigo SET server_id=?, server_created_at=?, '
                            + 'server_updated_at=?, user_id=?, mode=?,sync_flag=1 WHERE local_id=?',
                        row: [note.server_id, util.getUTCString(note.server_created_at),
                            util.getUTCString(note.server_updated_at), res.user_id,
                            note.mode, note.local_id]
                    },function(){});
                    db.tx({name: 'transaction',
                        query:"UPDATE notes SET lastSyncHash = ? WHERE id = ?",
                        row: [hex_md5(note.title+note.content),note.local_id]
                    },function(){});
                    
                }

//				note.name = 'update_item';
//				if(note.type==1){
//					//format cleantext to html
//					note.content = note.content.replace(/\r/g,'');
//					note.content = note.content.replace(/\n/g,'<br />');
//				}else if(note.type==4){
//					//for mark rich text
//					note.content = '<div attr="rich"></div>'+note.content;
//				}
//
//				db.tx(note);
//				db.tx({name: 'transaction',
//					query: 'UPDATE diigo SET mode=?,server_updated_at=?,sync_flag=1 WHERE local_id=?',
//					row: [note.mode,util.getUTCString(note.server_created_at), note.local_id]
//				}, function(){});
				break;
			case 3:
			case 4:	
				db.tx({name: 'update_list', data: 'trash', id: note.local_id});
				db.tx({name: 'transaction', 
					query: 'UPDATE diigo SET server_id=-1, server_created_at=-1, server_updated_at=-1, '
						+ 'user_id=?, mode=?, sync_flag=-1 WHERE local_id=?',
					row: [res.user_id, note.mode, note.local_id]
				},function(){
                    callbackForSyncStep();
                });
				break;
			}
		}
		
		if (newNotes.length) 
			diigo.serverNew(newNotes, res.user_id, function(){ callback(res); });
		else{
            if(diigo.status != "syncItems"){
                callback(res);
            }
        }
	},
	
	uploadNewItems: function(callback) {
		// console.log(typeof callback);
        var that = this;
		db.tx({name:'upload_newItems'}, function(tx, ds) {
			// if no edited note, return
			if (!ds.rows.length){
				if((typeof callback)=='function'){
                    that.syncStatus = false;
					callback();
				}else{
					diigo.syncItems();
				}
			}else{

				diigo.status = 'uploadNew';
				diigo.request('uploadItems', {items: diigo.getItems(ds.rows)}, function() {
					diigo.status = '';
//					localStorage['diigo_upload_stamp'] = util.getUTCString('now');
					if((typeof callback)=='function'){
                        that.syncStatus = false;
						callback();
					}else{
						diigo.syncItems();
					}
				});
			}
		});
	},
	syncItems: function() {
		db.tx({name:'sync_items'}, function(tx, ds) {

			var items = diigo.getItems(ds.rows);

            diigo.status = "syncItems";
            diigo.conflictCount = 0;
			function sync(){
				var items_ = items.splice(0,diigo.SyncMAXItemNumber);
				if(items_.length>0){
					diigo.request('syncItems', {folder_server_id_0: items_}, function(){
						sync();
					});
				}else{
                    diigo.status = "";
                    diigo.uploadUpdateItems();
				}
			}
			sync();
			// diigo.request('syncItems', {folder_server_id_0: diigo.getItems(ds.rows)}, function(){
			// 	diigo.loadItems();
			// });
		});
	},
    uploadUpdateItems:function(){
        db.tx({name:'upload_updateItems'},function(tx,ds){
            if(!ds.rows.length){
                diigo.loadItems();
            }else{
                diigo.status = "uploadUpdateItems";
                diigo.request('uploadItems', {items: diigo.getItems(ds.rows)}, function() {
                    diigo.status = '';
                    diigo.loadItems();
                });
            }
        });
    },
	loadItems: function() {
		diigo.request('loadItems', 
			{	more: 'new', order: 'create', time: util.getUTCInt(localStorage['diigo_load_stamp']) || util.getUTCInt('now'), 
				item_type: 'note', count: 50 }, 
			function() {
				var uploadtime = util.getUTCInt(localStorage['diigo_load_stamp']);
				
				db.tx({name: 'transaction', 
					query: 'SELECT max(server_created_at) FROM diigo WHERE user_id=?', 
					row: [JSON.parse(localStorage['diigo']).user_id]}, 
					function(tx, ds) {
						var created = ds.rows.item(0)['max(server_created_at)'];
						if (!created || created == -1)
							created = util.getUTCString('now');
						
						localStorage['diigo_upload_stamp'] = util.getUTCString('now');
						localStorage['diigo_load_stamp'] = created;
						
						// after login/auth to diigo, load some old notes
						if (diigo.firstTime) {
							diigo.firstTime = false;
							diigo.loadOldItems();
						}
						else {
							//chrome.extension.sendRequest({name: 'sync_done'});
							diigo.loadMoreOld(uploadtime);
						}
					}
				);
			}
		);
	},
	loadMoreOld: function(uploadtime){
        var that = this;
		diigo.request('loadItems', 
			{	more: 'old', order: 'create', time: uploadtime || util.getUTCInt('now'), 
				item_type: 'note', count: 10 }, 
			function() {
                diigo.SyncFinish();

			}
		);
	},
	loadOldItems: function() {
		// There're chances server_created_at will equal -1 when user opens
		// QN first time or has no notes, QN create a empty note whose 
		// server_created_at=-1. So we use 'WHERE server_created_at>0'
        var that =this;
		db.tx({name: 'transaction', query: 'SELECT min(server_created_at) FROM diigo WHERE server_created_at>0 AND user_id=?', row: [JSON.parse(localStorage['diigo']).user_id]}, function(tx, ds) {
			var created = ds.rows.item(0)['min(server_created_at)'];
			console.log(created);
			if (!created || created == -1) 
				created = util.getUTCString('now');
			console.log(created);
			diigo.request('loadItems', 
				{	more: 'old', order: 'create', time: util.getUTCInt(created), item_type: 'note', count: 20 }, 
				function(res){
					if (!res.result.items.length || res.result.items.length<20) {
						localStorage['diigo_no_old_items_flag'] = '1';
					}
                    diigo.SyncFinish();
                    chrome.extension.sendRequest({name: 'load_old_done'});
				}
			);
		});
	},
	
	init: function() {
		console.log('signin')
		localStorage['service'] = 'diigo';
		if(!localStorage['diigo']){
			localStorage['diigo'] = '1';
		}
		if(!localStorage['diigo_upload_stamp'] || localStorage['diigo_upload_stamp'].length<2) localStorage['diigo_upload_stamp'] = '0';
		diigo.firstTime = true;
		
		// db.diigo();
		// diigo.uploadItems(diigo.syncItems);
//		diigo.uploadItems();
        diigo.SyncBegin();
	}
};

chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
	var name = request.name;
	tab = sender.tab;
	
	switch(name) {
	case 'insert_script':
		chrome.tabs.insertCSS(tab.id, {file:'css/content.css'}, function() { 
			chrome.tabs.executeScript(tab.id, {file: 'js/content.js'}, function() {
				chrome.tabs.sendRequest(tab.id, {name:'init'}); // to content.js
			});
		});
		break;
	case 'script_running':
		chrome.tabs.sendRequest(tab.id, {name:'init'});  // to content.js
		break;
	case 'process_image':
		util.processImage(request, sendResponse);
		break;
	case 'update_menu':
		var title = 'New Note';
		selection = request.selection;
		if (selection) {
			title = 'Add selection to note';
			selection = '<div><br /></div>'+selection;
		} 
		else {
			switch(request.status.replace('diigo_note_app_', '')) {
			case 'maximize':
				title = 'Hide Note';
				break;
			case 'minimize':
			case 'close':
				title = 'Show Note';
				break;
			}
		}
		
		chrome.contextMenus.update(util.menu(), {title: title});
		//util.tabChange();
		break;
	case 'get_selection':
		chrome.tabs.sendRequest(tab.id, {name:'get_selection', selection:selection});
		break;
	case 'action': //from content.js(content script) to note.js(iframe)
		chrome.tabs.sendRequest(tab.id, {name:'action', action:request.action}); 
		break;
	case 'open_app':
		chrome.tabs.create({url:'app.html', index:tab.index+1});
		break;
	// case 'get_google_search':
	// 	$.get("http://ajax.googleapis.com/ajax/services/search/web?v=1.0&rsz=large&start=10&q=" + encodeURIComponent(request.keywords), {}, function(data){
	// 		sendResponse(data);
	// 	});
	// 	break;
	// case 'save_kw':
	// 	if(request.keywords.length>3){
	// 		db.tx({name:"delete_kw",kw:request.keywords},
	// 			function(tx,ds){
	// 				db.tx({name:"add_kw",kw:request.keywords},function(tx2,ds2){});	
	// 		});
			
			
	// 	}
	// 	break;
	// case 'get_everyfeed':
	// 	db.tx({name:"select_kw_new_10"},
	// 		function(tx,ds){
	// 			if(ds.rows.length>0){
	// 				var rand = parseInt(Math.random()*ds.rows.length);
	// 				var kw = ds.rows.item(rand).kw;
	// 				console.log(rand,kw);
	// 				$.get('http://api.everyfeed.net/json/1f9cb136f829fe2625a9e25c4c4154ab/search/'+kw+'/sort/price',function(data){
	// 					sendResponse(data);
	// 				});
	// 			}
	// 	});
	// 	break;
	default:
		db.tx(request, function(tx, ds) {
			var req = {name:name};
			
			switch(name) {
			case 'check_url':
				if (ds.rows.length) {
					req.item = ds.rows.item(0);
					req.selection = selection;
				}
				break;
			case 'add': 
				req.id = ds.insertId;
				req.selection = selection;
				if (localStorage['diigo']) {
					tx.executeSql('UPDATE diigo SET user_id=? WHERE local_id=?', 
						[JSON.parse(localStorage['diigo']).user_id, req.id]);
				}
				//if (localStorage['diigo'])
					//tx.executeSql('INSERT INTO diigo (local_id, server_updated_at) VALUES (?, DATETIME());', [ds.insertId]);
				break;
			case 'load_notes': 		// for app.js
				req.rows = ds.rows;
				break;
			}
			util.sendRequest(sender.tab.id, req);  	// to note.js
			
			notesUpdated = false;
		});
		break;
	}
});

function init() {
	db.open();
	
	// check if diigo table not exist
	db.tx({ 
		name:'transaction', 
		query:'SELECT name FROM sqlite_master WHERE type="table" AND name="diigo"',
		row:[]},
		function(tx, ds) {
			if (!ds.rows.length)
				db.diigo();
            else{
                db.tx({
                        name:"transaction",
                        query:"SELECT sql FROM sqlite_master WHERE tbl_name = 'diigo' AND type='table' ",
                        row:[]},
                    function(tx,ds){
                        var sql = ds.rows.item(0).sql;
                        if(sql.indexOf("sync_flag")<0){   //determine whether the field exists
                            db.insertSyncFlag();
                        }
                    }
                );

                db.tx({
                    name:"transaction",
                    query:"SELECT sql FROM sqlite_master WHERE tbl_name = 'notes' AND type='table' ",
                    row:[]},
                    function(tx,ds){
                        var sql = ds.rows.item(0).sql;
                        if(sql.indexOf("lastSyncHash") < 0){
                            db.upgrade_3(tx);
                        }
                    }
                );
            }
		}
	);
	
//	db.tx({
//		name:'transaction',
//		query:'SELECT name FROM sqlite_master WHERE type="table" AND name="kw"',
//		row:[]},
//		function(tx, ds) {
//			if (!ds.rows.length)
//			db.kw();
//		}
//	);


	
	var d = new Date();
	console.log(d.getTime());
	util.menu();
	Ext.onSelectionChanged();
	util.tabChange();
	
	/* setTimeout(function() {
		var d = new Date();
	console.log(d.getTime());
		util.menu();
		Ext.onSelectionChanged();
		util.tabChange();
	}, 1000); */
	
	
	// sync
	/* if (localStorage['google']) 
		util.require('js/sync.google.js'); */
}


chrome.tabs.onActivated.addListener(function(selecinfo){
    chrome.tabs.get(selecinfo.tabId,function(tab){
        if(tab.url == chrome.extension.getURL('app.html')){
//            console.log("is quick note");
            reloadCurrentTabNotes();
        }else{
            reloadSidebarNote(selecinfo.tabId,tab.url);
        }
    });
});

chrome.windows.onFocusChanged.addListener(function(winId){
    chrome.tabs.getSelected(function(tab){
        if(tab.url == chrome.extension.getURL('app.html')){
            reloadCurrentTabNotes();
        }else{
            reloadSidebarNote(tab.id,tab.url);
        }
    });
});

function reloadCurrentTabNotes(){
    var views = chrome.extension.getViews({type:"tab"});
    for (var i= 0,len=views.length;i<len;i++){
        var win = views[i];
        if(win.reloadNotes){
            win.reloadNotes();
        }
    }
}

function reloadSidebarNote(tabId,url){
    var url = url.replace(/#.*?$/,"");
    db.tx({name:"check_url",url:url},function(tx,ds){
        var req = {name:'raloadNote'}
        if(ds.rows.length){
            req.item = ds.rows.item(0);
            try{
                util.sendRequest(tabId,req);
            }catch(e){
                console.log("error");
            }
        }
    });
}

init();