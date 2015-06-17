git checkout master &&
    git pull &&
    git branch -D gh-pages &&
    git checkout -b gh-pages &&
    lein doc &&
    git add -f doc &&
    git commit -m "Update docs" &&
    git push -f origin gh-pages &&
    git checkout master
