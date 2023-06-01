# gm-ladder-webservice
Backend webservice using Persistent, Servant, HSpec and more.

## TODO

- Refactor code repetition in Api.hs and DbQueries.hs 

## Description

First and foremost, if anybody has any feedback or pointers into how to improve this project I would love to hear it.  Please leave a PR or Email me on mau5mat@gmail.com!

This project was an exercise in learning Persistent/Servant/HSpec.  I also wanted to gain some more context for some advanced Haskell topics (such as Monad Transformer stacks) to better learn the langauge and become more familiar with a lot of common industry-used libraries and techniques.

The project has a few main goals :-

1.  Make an HTTP request to a Blizzard Endpoint for some JSON data.
2.  Parse and transform the API Response into something that can be stored in an SQL Database using Persistent.
3.  Create some Domain logic to operate on the Database entities.
4.  Use the results of said logic to serve relevant data as JSON to certain Endpoints created with Servant.
5.  Have some rudimentary testing in place to catch some easy bugs.

I'm quite happy with how it turned out so far - I might plan to add more Endpoints and extend the functionality.  

For the future I plan to use this as a backend for Elm, and SwiftUI apps.


## Authors

Matt Roberts

## Contributors

[@EdmundNoble](https://github.com/edmundnoble])

## Version History

* 0.1
    * Initial Release

## Acknowledgments

A big thanks to the FP Discord for continually being a compassionate source of help and learning, and a special thank you to Edmund Noble, for taking time out of his day to help me troubleshoot some issues I had with Servant.

* [Hspec Documentation](https://hspec.github.io/)
* [Persistent Documentation](https://www.yesodweb.com/book/persistent)
* [Servant Documentation](https://docs.servant.dev/en/stable/tutorial/index.html)
* [Matt Parsons Blog Post](https://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html)
