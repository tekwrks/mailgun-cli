# mailgun-cli

[![Build Status](https://travis-ci.org/tekwrks/mailgun-cli.svg?branch=master)](https://travis-ci.org/tekwrks/mailgun-cli)

Command line tool to easily send emails through Mailgun

accepts mustache variables from:
  1. configuration file in ```variables``` object
  2. commands line as ```key=value```

- persistent configuration file
- mustache templates
- all options accessible through arguments and config file for easy automation
- interactive by default, fully scriptable (use --no-interaction)

