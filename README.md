Mudbath is continuous deployment server which integrates with GitHub. It
listens for deployment events and when it receives one, executes a shell
script. It reports progress back to GitHub in the form of deployment status
updates.

Mudbath can also send notifications to [Slack][slack] if the proper keys
are provided. Other notification sinks can be easily added if needed.


### Configuration

Mudbath has a GitHub webhook handler set up at `/webhook`. By default it
listens on port 8000, but you can change that with the `--port` commandline
argument.

Service configuration is done through environment variables.

If you want the progress to be reported back to github, set
`GITHUB_ACCESS_TOKEN` to a [personal API token][github-personal-api-token].

To send progress updates to [Slack][slack], set `SLACK_TEAM` to the team name
and `SLACK_TOKEN` to the API token from the [incoming
webhook][slack-incoming-webhook] section of Slack.


[slack]: https://slack.com
[github-personal-api-token]: https://github.com/blog/1509-personal-api-tokens
[slack-incoming-webhook]: https://my.slack.com/services/new/incoming-webhook
