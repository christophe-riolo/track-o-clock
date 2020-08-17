# pesy-reason-template

This the template used by pesy to bootstrap Reason projects. Since it contains template variables, it cannot be used on it's own


If you are looking for a template repo to bootstrap your Reason project, you can

1. Try [pesy](https://github.com/esy/pesy), or
2. Fork [hello-reason](https://github.com/esy-ocaml/hello-reason)
oject root to install and build depenencies.

    % esy

Now you can run your editor within the environment (which also includes merlin):

    % esy $EDITOR
    % esy vim

Alternatively you can try [vim-reasonml](https://github.com/jordwalke/vim-reasonml)
which loads esy project environments automatically.

After you make some changes to source code, you can re-run project's build
again with the same simple `esy` command.

    % esy

And test compiled executable (runs `scripts.tests` specified in
`package.json`):

    % esy test

Documentation for the libraries in the project can be generated with:

    % esy doc
    % open-cli `esy echo '#{self.target_dir}/default/_doc/_html/index.html'`
    
This assumes you have a command like [open-cli](https://github.com/sindresorhus/open-cli) installed on your system.

Shell into environment:

    % esy shell

## Create Prebuilt Release:

`esy` allows creating prebuilt binary packages for your current platform, with
no dependencies.

    % esy npm-release
    % cd _release
    % npm publish
