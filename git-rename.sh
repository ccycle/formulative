git filter-branch --force --env-filter '
        # GIT_AUTHOR_NAMEの書き換え
        if [ "$GIT_AUTHOR_NAME" = "Masatoshi Furuki" ];
        then
                GIT_AUTHOR_NAME="ccycle";
        fi
        # GIT_AUTHOR_EMAILの書き換え
        if [ "$GIT_AUTHOR_EMAIL" = "mf.0224j@gmail.com" ];
        then
                GIT_AUTHOR_EMAIL="ccycle713@gmail.com";
        fi
        # GIT_COMMITTER_NAMEの書き換え
        if [ "$GIT_COMMITTER_NAME" = "Masatoshi Furuki" ];
        then
                GIT_COMMITTER_NAME="ccycle";
        fi
        # GIT_COMMITTER_EMAILの書き換え
        if [ "$GIT_COMMITTER_EMAIL" = "mf.0224j@gmail.com" ];
        then
                GIT_COMMITTER_EMAIL="ccycle713@gmail.com";
        fi
        ' -- --all