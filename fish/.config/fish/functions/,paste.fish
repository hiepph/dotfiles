function ,paste --description "Paste the clipboard content"
    switch (uname)
        case Darwin
            pbpaste
        case Linux
            xsel --clipboard --output
    end
end
