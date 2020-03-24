# toggl_revery


[![CircleCI](https://circleci.com/gh/yourgithubhandle/toggl_revery/tree/master.svg?style=svg)](https://circleci.com/gh/yourgithubhandle/toggl_revery/tree/master)


**Contains the following libraries and executables:**

```
toggl_revery@0.0.0
│
├─test/
│   name:    TestTogglRevery.exe
│   main:    TestTogglRevery
│   require: toggl_revery.lib
│
├─library/
│   library name: toggl_revery.lib
│   namespace:    TogglRevery
│   require:
│
└─executable/
    name:    TogglReveryApp.exe
    main:    TogglReveryApp
    require: toggl_revery.lib
```

## Developing:

```
npm install -g esy
git clone <this-repo>
esy install
esy build
```

## Running Binary:

After building the project, you can run the main binary that is produced.

```
esy x TogglReveryApp.exe 
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```
