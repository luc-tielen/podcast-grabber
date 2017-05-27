
# Podcast-grabber

An utility to fetch episodes from your podcast subscriptions (RSS-based).


## Building

```bash
make
make install
```


## Using this utility

NOTE: requires having build the executable first (see above)


```bash
# Add a podcast feed
podcast-grabber --add http://www.haskellcast.com/feed.xml

# Look for new episodes and download them to ~/.podcast-grabber
podcast-grabber --update

# Remove a podcast feed
podcast-grabber --rm http://www.haskellcast.com/feed.xml
```

