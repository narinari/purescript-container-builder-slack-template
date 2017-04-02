# purescript-container-builder-slack-template

This is sample that integrate Google Container Builder notification and Slack with PureScript!

## Setup

### Install Google Cloud SDK and enable APIs.

[Google Cloud SDK](https://cloud.google.com/sdk/docs/) install.

Enable dependencies below:

- [Container Builder](https://cloud.google.com/container-builder/docs/quickstart-gcloud)
- [Cloud Function](https://cloud.google.com/functions/docs/quickstart)

### Install Node.js

[Node.js](https://nodejs.org/) or [NVM](https://github.com/creationix/nvm) install.

nvm:

    $ curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh | bash
    $ nvm install 4
    $ nvm alias default 4

confirm version:

    $ node -v
    > v4.4.3

### Install Yarn (option)

[YARN](https://yarnpkg.com/) download and install.

### Install dependencies

    $ yarn install
    $ yarn run psc-package -- install

### Compile

    $ yarn run build

### Register functions to Cloud Functions

    $ yarn run function:register
