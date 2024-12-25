


```sh


./mill clean

./mill __.reformat

./mill regenesca.test

./mill example.run

# for local dev/test
./mill  regenesca.publishLocal


# RELEASE
# bump regenescaVersion to x.y.z !!!
$VERSION="x.y.z"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main --tags


# prepare for NEXT version
# bump regenescaVersion to x.y.z-SNAPSHOT
$VERSION="x.y.z-SNAPSHOT"
git commit -am"Bump version to $VERSION"


```