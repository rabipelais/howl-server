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
be sure the following variables are in the environment:
```
APP_SECRET            Facebook app secret key
APP_ID                Facebook app ID
APP_NAME              Facebook app name (Howl)
AWS_ACCESS_KEY_ID     AWS secret key ID
AWS_ACCOUNT_ID        AWS account ID
AWS_DEFAULT_REGION    AWS region (eu-central-1)
AWS_REPO_NAME         AWS docker repo (howl-docker-repo)
AWS_S3_LOCATION       AWS S3 bucket location
AWS_SECRET_ACCESS_KEY AWS secret key
AMQP_HOST             RabbitMQ Host
AMQP_VHOST            RabbitMQ Virtual Host
AMQP_USER             RabbitMQ User
AMQP_PASSWORD         RabbitMQ password
DB_NAME               DB name
DB_USER               DB user
DB_HOST               DB host
DB_PASSWORD           DB password
DB_PORT               DB port (5432)
DB_POOLSIZE           DB connection pool size
```
In `bash` you do this with `$ export APP_NAME="example"`, in fish with `$ set -g -x APP_NAME "example"`

## Documentation
To auto-generate the `Swagger` JSON for the Web API, run `stack exec howl-swagger`
**TODO** Put here the actual API docs.

## Questions and Issues
Use the [Gitlab issue tracker](https://gitlab.com/smendez/howl-backend/issues) to submit bug reports and feature requests.

## License
Copyright (c) 2016-2017 Sebastian Mendez Siem
Licensed under the BSD-3 license.