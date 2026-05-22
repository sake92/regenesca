

```shell

deder clean

deder -t test

deder -t run -m hello
deder -t run -m migration


# for local dev/test
deder -t publishLocal



# RELEASE
VERSION="0.8.0"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main $VERSION

```