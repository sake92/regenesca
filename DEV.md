


```sh


./mill clean

./mill mill.scalalib.scalafmt/

./mill regenesca.test

./mill examples.hello.run
./mill examples.migration.run


# for local dev/test
./mill  regenesca.publishLocal



# RELEASE
VERSION="0.7.0"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main $VERSION


```