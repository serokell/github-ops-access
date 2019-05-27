# GitHub Ops Access

_This is a WIP prototype._

This is a webhook daemon that makes sure that any repository created in our
GitHub organisation has the Operations team as admins. It does this by authenticating
as a [GitHub App][ghapps], listening to the repsitory creation even, and adding
the team.


# Use

It uses the [`github-app`][github-app] library for authentication, therefore
see its documentation on how to start with registering your app.

Then set the following environment variables:

* `GITHUB_WEBHOOK_SECRET` (can be empty)
* `GITHUB_APP_ID`
* `GITHUB_APP_PK`

And, finally, run the `webhook` executable.

[ghapps]: https://developer.github.com/apps/
[github-app]: https://github.com/serokell/github-app/


## About Serokell

This library is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.
