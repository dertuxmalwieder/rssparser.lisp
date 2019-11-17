# rssparser.lisp

A Web-to-RSS parser in Common Lisp.

## What does it do?

This software was written because a disappointing number of websites still does not have an RSS or Atom feed so I could subscribe to their updates, e.g. the [KiTTY website](https://www.9bis.net/kitty/?action=news&zone=en). The script tries to find new *articles* on any website according to given criteria (CSS selectors) and parse them into a valid RSS feed so I can subscribe to them in my usual RSS feed reader.

## Screenshot

![Screenshot](http://i.imgur.com/fzkvW9H.png)

## Syntax

* *chmod +x rssparser.lisp*, then:
  * ./rssparser.lisp add *<Title> <URL> <EntrySelector> <TitleSelector> [<ContentSelector>]*
  * ./rssparser.lisp delete *<ID>*
  * ./rssparser.lisp list
  * ./rssparser.lisp export *<ID>*

***Run a simple web interface on port 5000:***

* ./rssparser.lisp webserver

***Cronjob or manual feed creation command:***

* ./rssparser.lisp parse

Supported *selectors* are all valid [CSS selectors](http://www.w3schools.com/cssref/css_selectors.asp). If you don't specify a `ContentSelector` when adding a new feed, `rssparser.lisp` will use an empty item body.

### Example

If you want to subscribe to the KiTTY website, you can either use the web interface or perform the following commands:

    % ./rssparser.lisp add "KiTTY" "https://www.9bis.net/kitty/?action=news&zone=en" ".news" "h1" ""
    Success!

    % ./rssparser.lisp parse

    % ./rssparser.lisp list
    1 feed is set up:

    ID: 23  Title:        KiTTY
            URL:          https://www.9bis.net/kitty/?action=news&zone=en
            Last success: Sun, 27 Mar 2016 17:54:18 +0200

By default, the KiTTY website feed will be stored as `feeds/feed23.xml` then.

## Requirements

You'll need the files from this repository and [SBCL](http://www.sbcl.org) with [Quicklisp](http://www.quicklisp.org) set up. [SQLite3](https://sqlite3.org) should be available. Also, you should create a folder where your feed files should be created (`./feeds` by default). Hard links are allowed.

### Packages

Usually, Quicklisp should install the required packages for you. If you want to install them manually, `rssparser.lisp` currently requires these:

* `datafly`
* `hunchentoot`
* `cl-who`
* `parenscript`
* `smackjack`
* `lass`
* `cl-ppcre`
* `dexador`
* `clss`
* `plump`
* `plump-sexp`
* `local-time`
* `xml-emitter`

### SQLite schema

The `feeds.db` file has the following schema:

    CREATE TABLE feeds (
      id integer primary key autoincrement,
      feedtitle text not null,
      url text not null,
      entryselector text not null,
      titleselector text not null,
      contentselector text not null,
      lastsuccess integer
    );

    CREATE TABLE entries (
      id integer primary key autoincrement,
      feedid integer,
      title text not null,
      contents blob,
      url text not null,
      timestamp integer
    );

### Exporting feeds into a new database

If you want to transfer one or more of your stored feeds into a new database, that's what the `export` command is for:

    % ./rssparser.lisp export 23
    Execute this SQL command to add this feed to a new database:
      INSERT INTO feeds ('feedtitle', 'url', 'entryselector', 'titleselector', 'contentselector') VALUES ('KiTTy', 'https://www.9bis.net/kitty/?action=news&zone=en', '.news', 'h1', '');

## Configuration

You can set a couple of parameters in the `config.lisp` file:

* `+database-file+`: The SQLite database file. (Default: `feeds.db`.) Note that this file *needs* to be accessible for the RSS parser to work!
* `+feed-folder+`: The folder where the feed files should be created. (Default: `feeds/`.) The script *needs* to be able to create files there; it checks its permissions automatically and informs you if it needs some help.
* `+max-items-per-feed+`: The maximum number of items per feed. (Default: `50`.)
* `+feed-cleanup+`: If set to `t` (which is the default value), the `entries` table will automatically be purged from old entries (only *2 * `+max-items-per-feed+`* are kept). Set this to `nil` if you want to bloat your database.
* `+remove-dead-feeds+`: If set to `t`, a website which is not reachable anymore will automatically be removed from your feed list. The parser will inform you of that so if you run `rssparser.lisp` as a cronjob, you'll see what happened in your logfiles.
* `+webserver-port+`: The port to run the webserver on when `rssparser.lisp webserver` is executed. It should be available through your firewall. (Default: `5000`.)
