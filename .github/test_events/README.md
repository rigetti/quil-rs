# GitHub Test Events

This folder contains mock GitHub webhook events. They can be used as input to [nektos/act](https://github.com/nektos/act) to simulate what will happen for that event. This is useful for testing CI changes locally.

For example, to simulate what will happen in GitHub actions run when a Python pre-release is published, you can run:

```sh
act release -n -e .github/test_events/prerelease_python.json
```

These payloads are adapted from GitHub's examples in [Events that trigger workflows](https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows).
