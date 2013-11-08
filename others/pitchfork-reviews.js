var http = require("http");
var Q = require("q");
var $ = require("jquery");


function downloadJQ(url) {
    var deferred = Q.defer();
    http.get(
	url, function(res) {
	    var body = "";
	    res.on("data", function(chunk) {
		body += chunk;
	    });

	    res.on("end", function() {
		deferred.resolve($(body));
	    })
	}
    );
    return deferred.promise;
}

function album(li_tag) {
    var $li_tag = $(li_tag);
    return {
	"artist": $li_tag.find("h1").text(),
	"title": $li_tag.find("h2").text(),
	"url": "http://pitchfork.com" + $li_tag.find("a").attr("href"),
	"score": null
    };
}

function trace(val) {
    console.dir(val);
    return val;
}

function downloadAlbums(page) {
    return downloadJQ("http://pitchfork.com/reviews/albums/" + page + "/").then(
	function($d) {
	    return $d.find("ul.object-grid ul li").map(function(i, el) { return album(el); });
	});
}

function downloadScore(a) {
    return downloadJQ(a['url']).then(
	function($d) {
	    return setScore(
		parseFloat($d.find("span.score").text()),
		a);
	});
}

function setScore(score, album) {
    album['score'] = score
    return album
}

function cmp(x,y) {
    if(x == y) { 
	return 0;
    } else if (x < y) {
	return -1;
    } else {
	return 1;
    }
}

function display(albums) {
    albums.forEach(function(result) {
	var album = result.value;
	if(album['score'] > 7) {
	    console.log(
		    album['score']
		    + " "
		    + album['artist'] 
		    + " - " 
		    + album['title']
		    + album['url']
	    )
	}
    });
}

function main() {
    downloadAlbums(1).then(function(albums) {
	return Q.allSettled(
	    albums.map(function(i, a) { return downloadScore(a) })
	).then(function(albums) {
	    display(albums.sort(function(y,x) { cmp(x['value']['score'], y['value']['score']) }));
	});
    }).fail(console.error);
}

main();
