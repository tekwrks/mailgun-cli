# mailgun-cli

![Version](https://img.shields.io/badge/version-1.0.0-brightgreen.svg)
[![Build Status](https://travis-ci.org/tekwrks/mailgun-cli.svg?branch=master)](https://travis-ci.org/tekwrks/mailgun-cli)

Command line tool to easily send emails through Mailgun.

## Features
- persistent configuration file
- mustache templates
- all options accessible through arguments **and** config file for easy automation and usage

## Installation

1. Running in Docker (preferred)
  ```
  docker run --rm \
    -v $(pwd)/config.yaml:/config.yaml:ro \
    -v $(pwd)/templates:/templates:ro \
    tekwrks/mailgun-cli:1.0.0 \
    --help
  ```

2. Building it locally
  ```
  git clone https://github.com/tekwrks/mailgun-cli
  cd mailgun-cli
  make && make install

  mailgun-cli --config=config.yaml
  ```

## Configuration
Accepts mustache variables from: (ascending priority)
  1. configuration YAML file in ```variables``` object
  2. flags field in YAML configuration file (as a list under ```flags```)
  3. command line arguments as ```key=value```

#### sample configuration
```YAML
config.yaml
---
domain: DOMAIN
apiKey: API-KEY

flags:
  - --subject="Hey World"
  - foo=bar

from: acme@acme.com
subject: Hello World
to:
  - email@domain.com
cc:
  - email@domain.com
  - email@domain.com
bcc:

plain:
  engine: mustache
  path: ./plain.mustache
html:
  path: ./html.mustache
variables:
  greeting: Hello
  name: World

...
```

## Options

All options are accessible also directly from command line options. (And will override values from configuration file!)

```
Usage: mailgun-cli [OPTION ...] [VARIABLE=VALUE ...]
                   --domain=DOMAIN           Mailgun DOMAIN
                   --api-key=API-KEY         Mailgun API-KEY
                   --from=email              sender email address
                   --subject=subject         email subject
                   --to=to                   email addresses, comma separated
                   --cc=cc                   email addresses, comma separated
                   --bcc=bcc                 email addresses, comma separated
  -c[config.yaml]  --config[=config.yaml]    yaml config file
                   --plain[=plain.mustache]  plain text mustache template
                   --html[=html.mustache]    plain text mustache template
  -h               --help                    Print this help message.
                   --version                 Print version.
```

## License

MIT License

