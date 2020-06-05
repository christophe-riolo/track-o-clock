# Track O'Clock


[![CircleCI](https://circleci.com/gh/christophe-riolo/track-o-clock/tree/master.svg?style=svg)](https://circleci.com/gh/christophe-riolo/track-o-clock/tree/master)


**Contains the following libraries and executables:**

```
trackoclock@0.0.0
│
├─test/
│   name:    TestTrackOClock.exe
│   main:    TestTrackOClock
│   require: trackoclock.lib
│
├─library/
│   library name: trackoclock.lib
│   namespace:    TrackOClock
│   require:
│
└─executable/
    name:    TrackOClockApp.exe
    main:    TrackOClockApp
    require: trackoclock.lib
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
esy x TrackOClockApp.exe 
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```

## Bumping verions:

```
# Use bump-patch, bump-minor or bump-major
esy bump-minor
```
