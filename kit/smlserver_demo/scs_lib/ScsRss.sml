(* $Id$ *)

signature SCS_RSS =
  sig

    type channel_record = {
      title       : string,
      link        : string,
      description : string,
      language    : ScsLang.lang,
      webMaster	  : string,
      ttl	  : int
    }

    type item_record = {
      title       : string,
      link        : string,
      description : string,
      pubDate     : Date.date
    }

    val mk_rss : channel_record -> item_record list -> quot

  end (* of signature *)

structure ScsRss :> SCS_RSS =
  struct

    type channel_record = {
      title       : string,
      link        : string,
      description : string,
      language    : ScsLang.lang,
      webMaster	  : string,
      ttl	  : int
    }

    type item_record = {
      title       : string,
      link        : string,
      description : string,
      pubDate     : Date.date
    }

    fun mk_item (item:item_record) = `
      <item>
        <title>^(#title item)</title>
	<link>^(#link item)</link>
	<description>^(#description item)</description>
	<pubDate>^(Date.fmt "%a, %d %b %Y %H:%M:00 +200" (#pubDate item))</pubDate>
      </item>`


    fun mk_rss (channel:channel_record) items = `
<?xml version="1.0" encoding="usascii7" ?>
  <rss version="2.0">
    <channel>
      <title>^(#title channel)</title>
      <link>^(#link channel)</link>
      <description>^(#description channel)</description>
      <language>^(ScsLang.toString (#language channel))</language>
      <webMaster>^(#webMaster channel)</webMaster>
      <ttl>^(Int.toString (#ttl channel))</ttl>` ^^ 
foldl (fn (item,acc) => mk_item item ^^ acc) `` items ^^ `
    </channel>
  </rss>
`
  end (* of structure *)
