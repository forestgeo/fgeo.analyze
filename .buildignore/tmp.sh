git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Tidy documentation."
git add .
git commit -m "$message"

message="Tidy documentation.
* abund_index()"

git add .
git commit --amend -m "$message"
