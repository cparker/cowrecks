function yox_youtube(){var k=jQuery,o={singleVideo:/^http:\/\/(?:www\.)?youtube.com\/watch\?v=([^\&]+)(.*)?/,playlist:/^http:\/\/(?:www\.)?youtube.com\/(?:view_play_list|my_playlists)\?p=([^\&]+)(.*)?/,user:/^http:\/\/(?:www\.)?youtube.com\/user\/([^\?]+)(?:\?(.*))?/,search:/^http:\/\/(?:www\.)?youtube.com\/results\?(.*)/};this.getImagesData=function(g,p){function r(b){var i=[];if(a.filter){var h=a.filter.match(/\!([^,]+)/g),e=a.filter.match(/(,|^)[^\!,]([^,]+)/g);h=h?h.join("|").replace(/^\!/,"").replace(/\|\!/g, "|"):null;e=e?e.join("|").replace(/\|,/g,"|"):null;if(h)a.negativeFilterRegex=RegExp("(?:^|,)("+unescape(h)+")(?:,|$)");if(e)a.positiveFilterRegex=RegExp("(?:^|,)("+unescape(e)+")(?:,|$)")}jQuery.each(b,function(q,f){if(l==="playlist")f=f.video;var c;c=f.tags;if(!a.filter||!c)c=false;else{c=c.join(",");var m=a.negativeFilterRegex&&a.negativeFilterRegex.test(c);m||(m=a.positiveFilterRegex&&!a.positiveFilterRegex.test(c));c=m}if(!c){c=f.title;c={thumbnailSrc:f.thumbnail[a.hqThumbnails?"hqDefault":"sqDefault"], link:f.player["default"],media:{element:k("<div>",{className:"yoxview_element",html:"<object width='100%' height='100%'><param name='movie' value='"+(f.content["5"]+"&fs=1&hd=1")+"'</param><param name='allowFullScreen' value='true'></param><param name='wmode' value='transparent'></param><param name='allowScriptAccess' value='always'></param><embed src='"+(f.content["5"]+"&fs=1&hd=1")+"' type='application/x-shockwave-flash' allowfullscreen='true' allowscriptaccess='always' wmode='transparent' width='100%' height='100%'></embed></object>"}), title:c,contentType:"flash",elementId:f.id,description:f.description}};k.extend(c.media,s(!!f.aspectRatio&&f.aspectRatio==="widescreen"));i.push(c)}});return i}var j=false,a=jQuery.extend({},{url:"http://gdata.youtube.com/feeds/api/videos",setThumbnails:true,setSingleAlbumThumbnails:true,alt:"jsonc",thumbsize:64,v:2,format:5,hqThumbnails:false,aspectRatio:"auto"},g.dataSourceOptions),l;if(g.dataUrl){var d;for(regexType in o)if(d=g.dataUrl.match(o[regexType])){l=regexType;break}if(d){switch(l){case "singleVideo":j= true;a.url+="/"+d[1];break;case "playlist":a.url="http://gdata.youtube.com/feeds/api/playlists/"+d[1];break;case "user":a.url="http://gdata.youtube.com/feeds/api/users/"+d[1]+"/uploads"}if(d=Yox.queryToJson(d.length==2?d[1]:d[2])){if(d.search_query){d.q=d.search_query;delete d.search_query}k.extend(a,d)}}}var s=function(){var b,i,h=16/9,e=false;if(!a.width&&!a.height)a.width=720;if(a.height&&!a.width||a.width&&!a.height){if(typeof a.aspectRatio==="string")if(a.aspectRatio==="auto")a.aspectRatio=4/ 3;else{e=a.aspectRatio.split(":");a.aspectRatio=parseInt(e[0],10)/parseInt(e[1],10)}e=a.aspectRatio===16/9;if(a.height){b={height:a.height,width:a.height*h};e||(i={height:a.height,width:a.height*a.aspectRatio})}else{b={width:a.width,height:a.width/h};e||(i={width:a.width,height:a.width/a.aspectRatio})}}return function(q){return q?b:i}}(),n={};g.onLoadBegin&&g.onLoadBegin();k.jsonp({url:a.url,data:a,async:false,callbackParameter:"callback",success:function(b){if(j&&!b.data||!j&&(!b.data.items||b.data.items.length=== 0))g.onNoData&&g.onNoData();else{n.contents=r(j?[b.data]:b.data.items);if(!j)if(b=b.data.title)n.title=b;p&&p(n);g.onLoadComplete&&g.onLoadComplete()}},error:function(){g.onLoadError&&g.onLoadError("YouTube plugin encountered an error while retrieving data")}})}};