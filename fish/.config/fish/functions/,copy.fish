function ,copy --description "Copy the content to clipboard"
    switch (uname)
        case Darwin
            pbcopy
        case Linux
            xsel --clipboard
    end
end
