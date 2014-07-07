---
layout: default
title: mudbath - Configuration
---

# Configuration

Mudbath is in essence a web server which handles a very special type of
requests. Therefore the port where it accepts those requests must be publicly
accessible.

By default it uses port 8080, but you can change it with the `--port` commandline
option.

Next you have to configure your github project to send deployment
notifications to mudbath. In the *webhooks* settings of the github project add
a new webhook and enter the full url to the mudbath server. The path must be
`/webhook`, ie. the full url looks something like this:

```
http://your.host.tld:8080/webhook
```

When Mudbath receives a deployment request from GitHub, it will try to run an
executable located at `$PWD/config/<user>/<project>/<environment>`. Mudbath
doesn't care in which language the executable is written.


## Deployment status updates

To save the deployment status updates on GitHub, you need to make a
[personal API token][github-personal-api-token] available to mudbath through
the `GITHUB_ACCESS_TOKEN` environment variable.


## Notifications

When a deployment is started or completed, mudbath can send updates to external services.

To send these updates to [Slack][slack], set `SLACK_TEAM` to the team name
and `SLACK_TOKEN` to the API token from the [incoming
webhook][slack-incoming-webhook] section of Slack.

Other external services are not supported, but are easy enough to add. Just
ping the author or even better write the code and open a pull request.


[slack]: https://slack.com
[github-personal-api-token]: https://github.com/blog/1509-personal-api-tokens
[slack-incoming-webhook]: https://my.slack.com/services/new/incoming-webhook


## Example

<ul class="command-sequence">
    <li class="cmd">GITHUB_ACCESS_TOKEN=AAA \
  SLACK_TEAM=awsm SLACK_TOKEN=BBB \
  mudbath --port 8080</li>
    <li class="out">Listening on http://0.0.0.0:8080/
[... skip ...]
[07/Jul/2014:04:54:19 +0000] Server.httpServe: START, binding to [http://0.0.0.0:8080/]</li>
</ul>
