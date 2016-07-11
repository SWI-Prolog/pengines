# A Prolog Engine application server

This repository provides a demo application server for the
[SWI-Prolog](http://www.swi-prolog.org)
[pengines
package](http://www.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/pengines.html%27%29).
An online version of this server is available at
http://pengines.swi-prolog.org.

The application server currently hosts two applications:

  * *Scratchpad* allows you to create interactive web applications that
  query Prolog. This demonstrates how pengines are supposed to be used:
  Prolog acts as a modular _query language_, while JavaScript handles
  the results of Prolog queries to vizualise the result.

  * *SWISH* provides a Prolog environment for trying Prolog, teaching,
  make a piece of code available for inspection to others,
  collaboratively solve a problem, etc. SWISH runs also as a
  [stand-alone server](http://swish.swi-prolog.org)

## Local installation installation

Running this software typically requires the latest _development
version_ of SWI-Prolog. After cloning the pengines
[repository](https://github.com/SWI-Prolog/pengines/), the server
can be started by loading `run.pl` into SWI-Prolog.  On MS-Windows,
this implies opening `run.pl` in the explorer.  On Unix systems, run

    swipl run.pl

By default, only *Scratchpad* is available.  See below for adding SWISH.


### Installing SWISH locally

First, add the swish submodule by running this command

    git submodule update --init

Next, install [bower](http://bower.io/) for your platform, go to
the directory `apps/swish` and run

    bower install
    make src

Finally, restart the pengines server.

### Docker

A [Docker](https://www.docker.com/) file is available from
https://github.com/ninjarobot/pengines-dockerfile

