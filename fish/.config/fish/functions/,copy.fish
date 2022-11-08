function ,copy --description "Copy the content to clipboard"
    tr -d '\n' | xsel --clipboard
end
