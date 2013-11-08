import gevent
from gevent import monkey; monkey.patch_all()
from gevent.pool import Pool
from pyquery import PyQuery as pq
import sys
from itertools import imap, ifilter
from pprint import pprint
import urllib2

def album(el):
    li_tag = pq(el)
    return {
        "artist": li_tag("h1").text(),
        "title": li_tag("h2").text(),
        "url": "http://pitchfork.com" + li_tag("a").attr("href"),
        "score": None,
    }

def downloadPQ(url):
    return pq(urllib2.urlopen(url).read())

def downlaadAlbums(page):
    return imap(
        album, 
        downloadPQ("http://pitchfork.com/reviews/albums/{page}".format(page=page))("ul.object-grid ul li")
    )

def downloadScore(url):
    try:
        return float(downloadPQ(url)("span.score").text())
    except:
        return None


def setScore(score, album):
    album['score'] = score
    return album


def display(albums):
    for album in albums:
        print "{score} {artist} - {title} {url}".format(**album)


def main():
    try:
        page = int(sys.argv[1])
    except:
        page = 1

    albums = list(downlaadAlbums(page))
    pool = Pool()
    scores = pool.map(lambda a: downloadScore(a['url']), 
                      albums)
    display(
        sorted(
            ifilter(
                lambda a: a['score'] > 7.0,
                imap(
                    setScore,
                    scores,
                    albums)),
            key=lambda a: a['score'],
            reverse=True
            ),
        )
                    
                    
if __name__ == '__main__': main()
