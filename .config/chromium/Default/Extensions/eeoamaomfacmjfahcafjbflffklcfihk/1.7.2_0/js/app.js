var bg = {}, db = {}, sv = ''; // sv: service
var getI18nMsg = chrome.i18n.getMessage;
var util = {
	months: [
		'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
		'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
	],
	include: function(loc) {
		var script = document.createElement('script');
		script.setAttribute('type', 'text/javascript');
		script.setAttribute('src', loc);
		document.head.appendChild(script);
	},
	isLink: function(link) {
			return (/http:|https:|ftp:/.test(link.split('/')[0]));
	},
	sendRequest: function(req) {
		chrome.extension.sendRequest(req);
	},
	getBG: function() {
		return chrome.extension.getBackgroundPage();
	},
	findP: function(t, sel) {
		return $(t).filter(sel).length ? t : $(t).parents(sel)[0];
	},
	localTimeFromUTC: function(time) { 					// http://stackoverflow.com/questions/3830418
		// get time offset from browser
		var currentDate = new Date();
		var offset = -(currentDate.getTimezoneOffset() / 60);
		// get provided date
		var timeArray = time.split(' ');
		var t0 = timeArray[0].split('-');
		var t1 = timeArray[1].split(':');
		var givenDate = new Date(t0[0], t0[1]-1, t0[2], t1[0], t1[1]);
		
		// apply offset
		var hours = givenDate.getHours();
		hours += offset;
		givenDate.setHours(hours);

		// format the date
		//var format = 'yyyy/MM/dd/ hh:mm a';
		var format = 'yyyy/MM/dd/HH:mm'; 				// this format made easy for manipulating, not for displaying
		var localDateString = $.format.date(givenDate, format);
		return localDateString;
	},
	time: function(time, mmhh) {
		time = util.localTimeFromUTC(time).split('/'); // [yyyy, MM, dd, HH:mm]
		var y	 = time[0],
				MM = time[1]-1, 
				dd = time[2],
				D  = new Date();
			
		return 	((y==D.getYear() && MM==D.getMonth() && dd==D.getDate()) 
							? 'Today' : util.months[MM]+' '+dd) // MM dd
						+
						(mmhh ? ' '+time[3] : ''); //mm HH
	},
	syncTime: function(time) {
		if (time == '0') return '';
		time = util.localTimeFromUTC(time).split('/');
		return time[0]+'-'+time[1]+'-'+time[2]+' '+time[3]
	},
	resize: function(e) {
		var initX = e.pageX;
		var delta = 0;
		var resizeX = parseInt($('#resize').css('left'));
		var headerX = parseInt($('#header').css('margin-left'));
		var notesX = parseInt($('#header').css('margin-left'));
		var navW = $('#nav').width();
		
		$('body').addClass('resizing')
			.mousemove(function(e) {
				delta = e.pageX-initX;
				$('#resize').css({left: (resizeX+delta)+'px'});
				$('#header').css({marginLeft: (headerX+delta)+'px'});
				$('#notes').css({marginLeft: (notesX+delta)+'px'});
				$('#nav').css({width: (navW+delta)+'px'});
			})
			.mouseup(function(e) {$(this).unbind('mousemove mouseup').removeClass('resizing');});
	},
	cleanStyles: function(e) {
		// function takes from readability
		if($(e).html().match('<div attr="rich"></div>')!=undefined){
		}
		else{
			e = e || document;
			var cur = e.firstChild;
	
			// If we had a bad node, there's not much we can do.
			if(!e)
				return;
	
			// Remove any root styles, if we're able.
			if(typeof e.removeAttribute == 'function')
				e.removeAttribute('style');
	
			// Go until there are no more child nodes
			while ( cur != null ) {
				if ( cur.nodeType == 1 ) {
					// Remove style attribute(s) :
					cur.removeAttribute("style");
					util.cleanStyles( cur );
				}
				cur = cur.nextSibling;
			}
		}
	},
	processImages: function(callback) {
		var imgs = $('img', $('#desc>div')),
				i = 0, img = imgs[i];
		if (!img) return callback();

		function processImage() {
			if (!util.isLink(img.src)) 
				return processNext();
			
			chrome.extension.sendRequest({
					name:'process_image',
					href:img.src,
					binary:true
				},
				function(res) {
						if (util.isLink(img.src)) img.dataset['src'] = img.src;
						img.src = 'data:' + res.contentType+ ';base64,' + toBase64(res.data);
						processNext();
				}
			);
		}
		function processNext() {
				if (img=imgs[++i]) processImage();
				else callback();
		}

		processImage();
	},
	fixImageSize: function (w) {
		$('img', $(w)).each(function(i) {
			console.log($(this).attr('width'));
			if (this.width>=650) 
				$(this).removeAttr('height');
		});
  },
	
	validate: function(user, pw) {
		var res = false;
		if (user && pw) {
			res = true;
		}
		else if (user && !pw) {
			$('#diigoAccount input[name=password]').focus().addClass('empty');
		}
		else if (!user && pw) {
			$('#diigoAccount input[name=username]').focus().addClass('empty');
		}
		else {
			$('#diigoAccount input[name=username]').focus().addClass('empty');
			$('#diigoAccount input[name=password]').addClass('empty');
		}
		
		return res;
	}
};

var note = {
	focusId: -1,
	titleTPL: function(row) {
		return '<li class="title" data-id='+row.id+'><h1><span>'+row.title+'</span><time>'+util.time(row.updated)+'</time></h1></li>';
	}, 
	num: function(n) {
		$('#num')[0].dataset['num'] = n;
		if ($('body').hasClass('trash')) {
			if (n>0) n = getI18nMsg('recycle')+'('+n+')';
			else n = getI18nMsg('recycle');
		} else {
			if (n>1) n = n+getI18nMsg('notes');
			if (n==1) n = '1'+getI18nMsg('note');
			if (n<1) n = getI18nMsg('note');
		}
		$('#num').html(n);
	},
	
	search: function(e) {
		var t = e.target;
        if(!t.value) return;
		var terms = '%'+t.value.replace(' ', '%')+'%';
        var list="";
        if($('body').hasClass('trash')) list = "trash";
        else if($('body').hasClass('conflict')) list = "conflict";
		db.tx({
			name:'search', 
			list: list,
			terms: terms
		}, note.result);
	},
	result: function(tx, ds) {
		var rows = ds.rows,
			len = rows.length,
			id;
		if (!len)
			return $('#nav>ul>li').hide() && note.num(0);
		
		$('#nav>ul>li').show().removeClass('show');
		for (var i=0; i<len; i++) {
			id = rows.item(i).id;
			$('li[data-id='+id+']').addClass('show');
		}
		$('#nav>ul>li:not(".show")').hide();
		note.num(len);
	},

	
	loadNav: function(list, callback) {	
		db.tx({name:'load_titles', list:list}, function(tx, ds) {
			note.buildNav(tx, ds);
			callback(ds); // ds for syncDone
		});
	},
	buildNav: function(tx, ds) {
		var ul = $('#nav>ul').empty()[0]/*  $('<ul></ul>').appendTo($('#nav').empty())[0] */,
			rows = ds.rows;
		
		note.num(rows.length);
		for (var i=rows.length-1; i>-1; --i) {
			ul.innerHTML += note.titleTPL(rows.item(i));
		}
		if (!rows.length && !$('body').hasClass('trash') && !$('body').hasClass('conflict')) {
			note.addNote();
			$('#noteWrapper').removeClass('empty-note');
		}
		//util.fixImageSize($(notes));
		//bg.notesUpdated = true;
	},
	clickNav: function(e) {
		
		var t = e.target, p;
			id = t.id;
		if(t.tagName=='SPAN' || t.tagName=="CITE"){
			if(t.id.length<1){
				id=$(t).parent().attr('id');
			}
		}
		switch (id) {
		case 'newBtn':								// new button
			note.addNote();
			break;
		case'emptyTrash': 
			if (confirm('Delete all notes in the trash?')) 
				db.tx({name:'empty_trash'}, function(tx, ds) {
					ttt = ds;
					$('#nav>ul').empty();
					note.emptyNote();
					note.num(0);
				});
			break;
        case 'emptyConflict':
            if (confirm('Delete all notes in the trash?'))
                db.tx({name:'empty_conflict'}, function(tx, ds) {
                    ttt = ds;
                    $('#nav>ul').empty();
                    note.emptyNote();
                    note.num(0);
                });
            break;
		case 'back':
            var lastin = "";
            if($('body').hasClass('trash')) lastin = "trash";
            else if($('body').hasClass('conflict')) lastin = "conflict";
			$('body').removeClass('trash').removeClass('conflict');
			note.loadNav('', function() {
                if(lastin == "conflict"){
                    $('#page-all').html($('#page-conflict').html());
                }else{
				    $('#page-all').html($('#page-trash').html());
                }
				$('#page-trash').html('');
                $('#page-conflict').html('');
				$('.active').removeClass('active');
				$('#page-all').addClass('active');
				note.loadFirstNote();
                $("#title input")[0].disabled = false;
                $("#desc div")[0].contentEditable = true;
				$('#search>input').bind('textchange', note.search);
			});
			break;
		case 'search':									// search
			note.search(e);
			break;
		case 'option':
			$('#sync_tips').animate({top:-80},1000).delay(1000).hide(0);
			$('#option>span').hide();
			$('body').addClass('scaling');
			
			setTimeout(function() {
				$('body').addClass('flipping');
				//$('body').addClass('flip');
			}, 125);
						
			if ($('body').hasClass('flip')) {
				$('body').addClass('hideFlipback');
				setTimeout(function() {
					$('body').removeClass('scaling flipping')
						.removeClass('flip showFlip hideFlipback');
				}, 350);
			}
			else {
				setTimeout(function() {
					$('body').addClass('showFlip');
				}, 250);
				setTimeout(function() {
					$('body').removeClass('scaling flipping')
						.addClass('flip');
				}, 350);
			}
			/* $('body:not(.flip)').addClass('flipping');
			setTimeout(function() {
				$('body').removeClass('flipping')
					.hasClass('flip') 
					? $('body').removeClass('flip')
					: $('body').addClass('flip');
			}, 500); */
			
/* 			$('body').addClass('option');
			$('#option').removeClass('show-tip');
			$('.active').removeClass('active');
			$('#page-option').addClass('active');
 */			break;
		case 'trash':
            if($('body').hasClass('trash')) return;
			$('body').addClass('trash'); 
			note.loadNav('trash', function() {
				$('#page-trash').html($('#page-all').html());
				$('#page-all').html('')
				$('.active').removeClass('active');
				$('#page-trash').addClass('active');
				note.loadFirstNote();
                $("#title input")[0].disabled = true;
				$("#desc div")[0].contentEditable = false;
				$('#search>input').bind('textchange', note.search);

			});
			
			break;
        case 'conflict':
            if($('body').hasClass('conflict')) return;
            $('body').addClass('conflict');
            note.loadNav('conflict',function(){
                $('#page-conflict').html($('#page-all').html());
                $('#page-all').html('')
                $('.active').removeClass('active');
                $('#page-conflict').addClass('active');
                note.loadFirstNote();
                $("#title input")[0].disabled = true;
                $("#desc div")[0].contentEditable = false;
                $('#search>input').bind('textchange', note.search);
            });
            break;
		case 'closePromoMsg':
			$('#promotions').remove();
			break;
		
		/* case 'diigo':
			$('#diigoAccount').jqm().jqmShow()
				.find('input[name=username]').blur(); // disable auto-focus on first input
			break; */
		
		case 'option-back':
			$('.active').removeClass('active');
			$('body').removeClass('option');
			$('#page-all').addClass('active');
			break;
		case 'google': 	
			bg.syncDoc();
			break;
		case 'syncBtn':
			note.syncStart();
			break;
		default:
			if (p = util.findP(t, '.title')) {
				note.loadNote(p.dataset['id'], function(){});
			}
			else if (util.findP(t, '#loadOldNotes')) {
				$('#nav').addClass('loading');
					bg.diigo.loadOldItems();
				if (localStorage['diigo_no_old_items_flag'] === '1') {
					$('#nav').addClass('hide-load-more');
				}
				else {
					$('#nav').addClass('loading');
					bg.diigo.loadOldItems();
				}
			}
		} 	
	},
	
	updateTitle: function() {
		var $p = $('#note');
		if (!$p.hasClass('new')) return;
		
		var text = $('#desc>div').text();
		text=text.replace(/>/g,'&gt;');
		text=text.replace(/</g,'&lt;');
		if (text.length>30) text = text.substr(0, 30);
		note.storeTitle(text, function() {
			text=text.replace(/&gt;/g,'>');
			text=text.replace(/&lt;/g,'<');
			$('#title>h1>input').val(text); /* text.length>30 ? text.substr(0, 30) : text */
			$('.title.focus>h1>span').text(text);
		});
	},
	storeTitle: function(text, callback) {
		db.tx({name:'title', data:text, id:note.focusId}, callback);
	},
	
	addNote: function() {
		$('#noteWrapper').removeClass('empty-note');
		if ($('.empty').length) 
				return note.loadNote($('.empty')[0].dataset['id'], function(){});
			
		db.tx({name:'add', from:'app'}, function(tx, ds) { 			// new note
			if (localStorage['diigo']) {
				tx.executeSql('UPDATE diigo SET user_id=?,sync_flag=-1 WHERE local_id=?',
					[JSON.parse(localStorage['diigo']).user_id, ds.insertId]);
			}
			
			note.loadNote(ds.insertId, function(tx, ds) {
				var row = ds.rows.item(0);
				$(note.titleTPL({id:row.id, title:getI18nMsg('newnote'), updated:row.updated}))
					.prependTo($('#nav>ul'))
					.addClass('focus empty');
					
				var num = parseInt($('#num').html());
				if (isNaN(num)) num = 0;
				note.num(parseInt($('#num')[0].dataset['num'])+1);
				$('#note').addClass('new');
			});
		});
	},
	emptyNote: function() {
		$('#title>h1>input').val('');
		$('#title>h1>time').html('');
		$('#desc>div').html('');
		$('#noteWrapper').addClass('empty-note');
		note.focusId = -1;
	},
	loadNote: function(id, callback) {
		db.tx({name:'load_note', id:id}, function(tx, ds) {
			$('#noteWrapper').removeClass('empty-note');
			
			$('li.focus').removeClass('focus');
			$('li.title[data-id='+id+']').addClass('focus');
			note.buildNote(tx, ds);
			note.focusId = id;
			//note.focusId = $('.title.focus').attr('data-id') || -1;
			callback(tx, ds);
		});
	},
	loadFirstNote: function() {
		var firstNote = $('#nav>ul>li:first-child')[0];
		firstNote ? note.loadNote(firstNote.dataset['id'], function(){
									$('#noteWrapper').removeClass('empty-note');
								}) // load first note
							: note.emptyNote();
	},
	buildNote: function(tx, ds) {
		var row = ds.rows.item(0);
		var title = row.title;
		title=title.replace(/&gt;/g,'>');
		title=title.replace(/&lt;/g,'<');
		$('#title>h1>input').val(title);
		$('#title>h1>time').html(util.time(row.updated, true));
		$('#desc>div').html(row.desc).focus().select();
        $('#contenttmp').html(row.desc);
		util.fixImageSize($('#desc>div')[0]);
	},
	clickNote: function(e) {
		var t = e.target, p; 
		
		if (util.findP(t, '#top') && t.tagName=='SPAN') {					// top
			var data;
			if (t.id=='remove') {
				data = 'trash';
			}
			if (t.id == 'restore') {
				data = '';
			}
			
			var id = note.focusId;
			db.tx({name:'list', id:id, data:data}, function(tx, ds) {	// delete note
				var $li = $('li.title[data-id='+id+']'),
					$nli = $li.next().length ? $li.next() :
						($li.prev().length ? $li.prev() : []);
				$nli.length ? note.loadNote($nli.attr('data-id'), function(){}) 
										: note.emptyNote();
				
				$li.remove();
				note.num(parseInt($('#num')[0].dataset['num'])-1);
				$(t).hide().delay(1500).fadeIn(500);
			});
		}
		else if (util.findP(t, '#desc')) {				// handle link
			var href = '';
			if (t.tagName=='A')
				href = $(t).attr('href');
			if ($(t).parents('a').length) 
				href = $(t).parents('a').attr('href');
			if (href) {
				window.open(href);
				return false;
			}
		}
		else if (util.findP(t, '#title')) {				//stop auto update title when user click on input
			$('#note').removeClass('new');	
		}
	},
	
	syncInfo: function() {
		$('body').addClass('syncEnabled');
		$('<div id="service-enabled">'
				+ '<span class="' + sv + '"><a title="Diigo Note" target="_blank" href="http://www.diigo.com/user/' 
				+ JSON.parse(localStorage[sv]).user
				+ '?dm=middle&type=note"><span>' + sv + '</span> (<span>' + JSON.parse(localStorage[sv]).user + '</span>)</a></span>'
				+ '<span id="syncStopBtn" class="button">'+getI18nMsg('stop')+'</span>'
				+ '<div class="sync-status">'+getI18nMsg('lastsync')
				+ 	util.syncTime(localStorage[sv+'_upload_stamp']) 
				+ '</div>'
			+	'</div>')
			.insertAfter($('#service-content'));
	},
	syncStart: function() {
//		$('#syncBtn').addClass('syncing').attr({title: getI18nMsg('syncing')});
		
		switch(sv) {
		case 'diigo':
			// bg.diigo.uploadItems(bg.diigo.syncItems);
//			bg.diigo.uploadItems();
               bg.diigo.SyncBegin();
			break;
		case 'google':
			bg.checkFolder();
			break;
		}
	},
	syncDone: function() {
		var time = getI18nMsg('lastsync') +util.syncTime(localStorage[sv+'_upload_stamp']);
		$('#syncBtn').removeClass('syncing').attr({title: time}); 
		$('.sync-status').html(time);
		note.loadNav('', function(ds) {
            var ids = [];
            $('li.title').each(function(i,n){ids.push(n.dataset['id'])});
            if(ids.indexOf(note.focusId)<0) note.focusId = ids[0];
			db.tx({name:'load_note', id:note.focusId}, function(tx, ds) {
				if(ds.rows.item(0).desc==''){
					db.tx({name: 'transaction', 
						query: 'SELECT max(notes.id) FROM notes,diigo WHERE user_id=? and list=""', 
						row: [JSON.parse(localStorage['diigo']).user_id]},
						function(tx,ds){
							note.loadNote(ds.rows.item(0)['max(notes.id)'],function(){});
							return;
						}
					);
				}
//                if(currentNoteId){
//                    note.loadNote(currentNoteId,function(){});
//                }
			});
			
			
			if (note.focusId != -1) {
				note.loadNote(note.focusId, function(){});
			}
			// FIXME - after sync with server, if a focused note being deleted
			// should find another one as focused
			/* if (ds.rows.length && note.focusId != -1) {
				console.log(note.focusId);
				console.log(ttt=ds);
				db.tx({name: 'transaction', 
					query: 'SELECT FROM notes WHERE id=?', row: [note.focusId],
					function(tx, ds) {
						if (ds.rows.item(0).list !== 'trash') {
							note.loadNote(note.focusId, function(){});
						}
						else {
							note.emptyNote();
						}
					}
				});
				
			}
			else {
				note.emptyNote();
			} */
		});
	},
	syncStop: function() {
		// note.backupUserInfo();
		var user_id 	= JSON.parse(localStorage['diigo']).user_id;
		localStorage['diigo'+user_id] = JSON.stringify({
			load_stamp: 	localStorage['diigo_load_stamp'],
			upload_stamp: localStorage['diigo_upload_stamp'],
			account:			localStorage['diigo']
		});
		
		localStorage['service'] = localStorage['diigo_load_stamp'] 
			= localStorage['diigo_upload_stamp'] = localStorage['diigo_no_old_items_flag'] 
			= localStorage['diigo'] = '';
		
		$('body').removeClass('syncEnabled');
		$('#service-enabled').remove();
		
		/* localStorage['service'] = '';
		$('body').removeClass('syncEnabled');
		$('#service-enabled').remove(); */
		/* db.tx({name: 'transaction', 
			query: 'DROP TABLE IF EXISTS '+sv, row: []}, function(tx, ds) {
				tx.executeSql('DROP TRIGGER IF EXISTS '+sv+'_insert');
				tx.executeSql('DROP TRIGGER IF EXISTS '+sv+'_delete');
				tx.executeSql('DROP TRIGGER IF EXISTS '+sv+'_update');
				
				var QNClickedMsgSlug = localStorage['QNClickedMsgSlug']; 
				localStorage.clear();
				localStorage['sync_tip'] = '1';
				localStorage['QNClickedMsgSlug'] = QNClickedMsgSlug;
				$('body').removeClass('syncEnabled');
				$('#service-enabled').remove();
			}
		); */
	},
	syncAuto: function() {
		// we fetch max note.updated time (maxUpdated) every 60s
		// divide (div) current time with maxUpdated
		// if (div > 60) uploadItems
		// else keep checking
		
		var maxUpdated, divide, interval = 1*60*1000, timeoutId = 0;
		
		function checkUpdated() {
//            console.log("autosync");
			bg.db.tx(
				{	name: 'transaction',
					query: 'SELECT max(updated) FROM notes',
					row: []
				},
				function(tx, ds) {
					if (ds.rows.length) {
						maxUpdated = ds.rows.item(0)['max(updated)'];
						maxUpdated = bg.util.getUTCInt(maxUpdated);
						divide = Date.parse(new Date()) / 1000 - maxUpdated;
						
						if (divide > interval / 1000) {
							bg.diigo.SyncBegin(function() {
								var time = getI18nMsg('lastsync')+util.syncTime(localStorage[sv+'_upload_stamp']);
								$('#syncBtn').removeClass('syncing').attr({title: time}); 
								$('.sync-status').html(time);
								
								timeoutId = setTimeout(checkUpdated, interval);
							});
						}
						else {
							timeoutId = setTimeout(checkUpdated, interval);
						}
					}
					else {
						timeoutId = setTimeout(checkUpdated, interval);
					}
				}
			);
		}
		
		// list request

		
		// init auto upload
		timeoutId = setTimeout(checkUpdated, interval);
	},
	syncInit: function() {
		if (!localStorage['sync_tip']) {
			$('#option').addClass('show-tip');
			localStorage['sync_tip'] = '1';
		}
		if (localStorage['diigo_no_old_items_flag'] === '1') {
			$('#nav').addClass('hide-load-more');
		}
		
		$('#diigoAccount .loginByGoogle .button')
			.attr({href: 'https://www.diigo.com/account/thirdparty/google'
				+ '?redirect_url='+'https://www.diigo.com/quicknotedone'
				+ '&request_from=chrome_quick_note'
			});
		
		// bind login
		function login() {
			var user = $('#diigoAccount .loginByDiigo input[name="username"]').val(),
					pw	 = $('#diigoAccount .loginByDiigo input[name="password"]').val();
			if (util.validate(user, pw)) {
				$('#diigoAccount').addClass('authing');
				bg.diigo.request('signin', {user: user, password: pw}, function(){});
			}
		}
		$('#diigoAccount .loginByDiigo .button').click(login);
		$('body').keyup(function(e) {
			if (e.keyCode === 13 && !$('body').hasClass('syncEnabled') 
				&& $('body').hasClass('flip')) {
				login();
			}
		});
		
		// bind input
		$('#diigoAccount').bind('keyup click', function(e) {
			if (e.target.tagName === 'INPUT') {
				$(e.target).removeClass('empty');
			}
		});
		//bind done button 
		$('#SettingDone').bind('click', function(e) {
			note.clickNav({target: {id: 'option'}});
		});
		//bind option page
		$('#page-option').click(function(e) {
			var t = e.target;
			
			if (t.id === 'syncStopBtn') {
				$('#stopSync').jqm().jqmShow();
			}
		});
		
		// listen request
		chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
			switch(request.name) {
			case 'network_error':
				$('#diigoAccount').removeClass('authing');
				$('#networkError').jqm().jqmShow();
				note.syncDone();
				break;
			case 'auth_failure':

				$('#diigoAccount').removeClass('authing');
				if (localStorage['service']) {
					$('#authError').jqm().jqmShow();
					
					$('body').removeClass('syncEnabled');
					$('#service-enabled').remove();
				}
				else {
					$('#diigoAccount').addClass('authFailure');
					setTimeout(function(){$('#diigoAccount').removeClass('authFailure')}, 4000);
				}
                note.syncStop();
				break;
			case 'after_auth':
				$('#option').removeClass('show-tip');
				$('#diigoAccount').removeClass('authing');
				sv = localStorage['service'];
				note.syncInfo();
				$('#syncBtn').addClass('syncing');
				note.clickNav({target: {id: 'option'}});
				break;
			case 'sync_done':
				note.syncDone();
                if(request.conflictCount && request.conflictCount > 0){
                    $('#conflict_tips span:eq(0)').html(request.conflictCount);
                    $('#conflict_tips').show(0).animate({top:-40},1000).delay(4000)
                        .animate({top:-80},1000).delay(1000).hide(0);
                }
				break;
			case 'load_old_done':
				$('#nav').removeClass('loading');
				if (localStorage['diigo_no_old_items_flag'] === '1') {
					$('#nav').addClass('hide-load-more');
				}
				else if (note.focusId === -1) {
					note.focusId = 1;
				}
				note.syncDone();
				break;
			}
		});
		
		sv = localStorage['service'];
		if (sv) {
			note.syncInfo();
			note.syncStart();
			note.syncAuto();
		}

        chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
            switch(request.name) {
                case 'show_syncing':
                    $('#syncBtn').addClass('syncing').attr({title: getI18nMsg('syncing')});
                    break;
            }
        });
	},

    reloadNote:function(){
        var currentNoteId = $("li.focus")[0] && $("li.focus")[0].dataset['id'];
        note.loadNav("",function(){
            if(currentNoteId){
                note.loadNote(currentNoteId,function(){});
            }else{
                note.loadFirstNote();
            }
        });
    }
}

//for mainfest2 move the inline-script to there

var UI={
	bindUI : function(){
		$('#sync_tips span:eq(0)').click(function(e){
			note.clickNav({target:{id:'option'}});
			$('#sync_tips').animate({top:-80},1000).delay(1000).hide(0);
		});
		$('#sync_tips span:eq(1)').click(function(e){
			$('#sync_tips').animate({top:-80},1000).delay(1000).hide(0);
		});
        $('#conflict_tips span:eq(1)').click(function(e){
            note.clickNav({target:{id:'conflict'}});
            $('#conflict_tips').animate({top:-80},1000).delay(1000).hide(0);
        });
        $('#conflict_tips span:eq(2)').click(function(e){
            $('#conflict_tips').stop().animate({top:-80},1000).delay(1000).hide(0);
        });
		$('#authError a').click(function(e){
			note.clickNav({target: {id: 'option'}});
		});
		$('#stopSync a:eq(0)').click(function(e){
			$('#stopSync').jqmHide();
		});
		$('#stopSync a:eq(1)').click(function(e){
			$('#stopSync').jqmHide(); note.syncStop(); $('#nav').removeClass('hide-load-more');
		});
	}
}


function reloadNotes(){
    note.reloadNote();
}

$(document).ready(function() {
	UI.bindUI();
	if(localStorage['diigo']){
		$('#sync_tips').hide();
	}
	var d = new Date();
	console.log(d.getTime());
	
	bg = util.getBG();
	db = bg.db;
	db.tx({name:'clean_empty_data'}, function(){});
	
	note.loadNav('', function() {
		note.loadFirstNote();
		$('#navSection').click(note.clickNav);
		$('#noteSection').click(note.clickNote);
		// when switch page (e.g all to trash), should rebind search
		$('#search>input').bind('textchange', note.search);
		$('#title>h1>input').bind('textchange', function(e) {
			var text = this.value;
			text=text.replace(/>/g,'&gt;');
			text=text.replace(/</g,'&lt;');
			note.storeTitle(text, function() {
				$('.title.focus>h1>span').html(text);
			});
			$('.focus.empty').removeClass('empty');	
		});
		$('#desc>div')
			.bind('textchange', function(e) {
				util.cleanStyles(this);
				util.fixImageSize(this);
                $('#contenttmp').html($('#desc>div').html());
				util.processImages(function() {
					db.tx({name: 'desc', id: note.focusId, data: $('#desc>div').html()}, function(){});
					note.updateTitle();
					$('.focus.empty').removeClass('empty');
				});
			})
			.keydown(function(e) {
				if (e.keyCode==13) $('#note').removeClass('new');
			});
		
		// sync
		note.syncInit();
		
		/* $('#option').click(function(e) {
			if ($(e.target).hasClass('back')) {
				$(e.target).removeClass('back');
				$('#noteSection').revertFlip();
				
			}
			else {
				$(e.target).addClass('back');
				$('#noteSection').flip({direction: 'lr', color:'transparent', content: $('#flipBackSection'), speed:300});
			}
		}); */
		
		// promotion
		// if ($('#nav>ul>li').length>3) {
		// 	$('<script></script>')
		// 		.attr({src: 'http://readict.com/promotion/script-for-QN.js'})
		// 		.appendTo($('head'));
		// }
		
		$('<script><\/script>').attr({src: 'js/settext.js'}).appendTo($('head'));
		
		if(getI18nMsg('local')=='zh_CN'){
		    $('.section:eq(1) ul li:eq(0)').hide();
		    $('.section:eq(1) ul li:eq(1)').hide();
		    $('.section:eq(1) ul li:eq(2)').hide();
		    $('.section:eq(1) ul li:eq(3)').hide();
		}
		
		if(localStorage['diigo']){
			var is_premium = JSON.parse(localStorage['diigo']).is_premium;
			if(!is_premium){
				 // addads();
			}
		}else{
			 // addads();
		}
	});
});









/* $('#google').click(function() {
	chrome.extension.getBackgroundPage().syncDoc();
}); */
//util.include('js/sync.google.js');

/*=======For ads=====*/
function addads(){
	console.log('addads');
	var adlisthtml = '';
	var html = '';
	
	html +='<div id="ads" style="display:none">';
	html +='<span id="closeAdsMsg"></span>';
	html +='<h4 class="promoHeader">More App from Diigo</h4>';
	html +='<div id="appSearchO" class="msgItem">';
	html +='<a target="_blank" href="https://chrome.g.com/webstore/detail/eekjldapjblgadclklmgolijbagmdnfk"><strong>SearchO</strong> The easiest way to access different search engines. »</a>'
	html +='</div></div>';
	$('#page-all').append(html);
	$('head').append('<link rel="stylesheet" href="css/ads.css" />');
	$('#closeAdsMsg').click(function(e){
		$('link[href="css/ads.css"]').remove();
	});

}


var everyfeed = {
	getrequest:function(res){
		if(res.everyFeed.status=='200'){
			var len = res.everyFeed.products.length<5?res.everyFeed.products.length:5;
			if(len<1) return;
			var rand = parseInt(Math.random()*len);
			var adlisthtml = '';
			adlisthtml +='<a href="'+URLads(res.everyFeed.products[rand].url)+'" target="_blank"><img src="'+res.everyFeed.products[rand].image_url+'" /></a></div><div id="adlinks">';
			/*for(i=0;i<len;i++){
				adlisthtml+='<a href="'+URLads(res.everyFeed.products[i].url)+'" target="_blank">'+res.everyFeed.products[i].title+'</a>';
			}*/
			adlisthtml+='<a href="'+URLads(res.everyFeed.products[rand].url)+'" target="_blank">'+res.everyFeed.products[rand].title+'</a>';
			
			this.buildhtml(adlisthtml);
		}
	},
	buildhtml:function(adlist){
		$('head').append('<link rel="stylesheet" href="css/ads.css" />');
		var html = '';
		html +='<div id="ads" style="display:none">';
		html +='<span id="closeAdsMsg"></span>'
		html +='<div id="adsimage">';
		//html +='<h4 class="adsHeader">Ads</h4>'
		//html +='<ul>';
		html += adlist;
		html +='</div></div>';
		$('#page-all').append(html);
		
		$('#closeAdsMsg').click(function(e){
			$('link[href="css/ads.css"]').remove();
		});
	}
	
	
}



