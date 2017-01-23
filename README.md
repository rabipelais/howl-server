# Howl Backend

Services for the backend serving the Howl API.

[![Build Status](https://gitlab.com/smendez/howl-backend/badges/master/build.svg)](https://gitlab.com/smendez/howl-backend/commits/master) [![coverage report](https://gitlab.com/smendez/howl-backend/badges/master/coverage.svg)](https://gitlab.com/smendez/howl-backend/commits/master)

## Building

To compile the source, be sure you have the Haskell Tool Stack. To install this, simply run `curl -sSL https://get.haskellstack.org/ | sh`.
Then run
```
$ stack setup
$ stack build
```

## Usage

To start the web-server, run
```
$ stack exec howl-backend
```

To run the tests, use
```
$ stack test howl-backend:howl-backend-test
```
be sure the variables `APP_NAME`, `APP_SECRET`, and `APP_ID` are in the environment. In bash you do this with `$ export APP_NAME=\"example\""`, in fish with `$ set -g -x APP_NAME \"example\"`

## Documentation
To auto-generate the `Swagger` JSON for the Web API, run `stack exec howl-swagger`
**TODO** Put here the actual API docs.

## Questions and Issues
Use the [Gitlab issue tracker](https://gitlab.com/smendez/howl-backend/issues) to submit bug reports and feature requests.

## License
Copyright (c) 2016-2017 Sebastian Mendez Siem
Licensed under the BSD-3 license.