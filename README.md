# elm2048

Working through the [tutorial](http://scrambledeggsontoast.github.io/2014/05/09/writing-2048-elm/) on creating a 2048 game using [Elm language](http://elm-lang.org).

It was a very nice read with good enough explanations to follow through every
single line of the tutorial (although I was already familiar with Elm after
following some of its tutorials on their site)

It turned out that my (newer) version of Elm had some API changes and it was a
nice extra task to make the code work with my version of Elm (0.13 at that time)

To test it out you need to have Elm installed and then you can execute these
steps to build and run the game:

    cd src
    elm -m -o --bundle-runtime Elm2048.elm
    cp index.html build/
    cd build
    open index.html
