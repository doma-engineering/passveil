# Bash completion for passveil

_passveil()
{
    local cur prev words cword
    _init_completion || return

    local commands="show search list info insert edit move delete allow deny distrust init sync undo"
    local opts="--store --version -h --help"

    # Parse options to check for '--store' and its argument
    local store=""
    local i
    for ((i=1; i < ${#COMP_WORDS[@]}; i++)); do
        if [[ "${COMP_WORDS[i]}" == "--store" ]]; then
            if [[ $((i+1)) -lt ${#COMP_WORDS[@]} ]]; then
                store="${COMP_WORDS[i+1]}"
            fi
        fi
    done

    # Get the current and previous words
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # If '--store' is the last word, complete directories
    if [[ "${prev}" == "--store" ]]; then
        _filedir -d
        return
    fi

    # If no subcommand is specified yet
    if [[ ${COMP_CWORD} -eq 1 ]] || [[ "${COMP_WORDS[1]}" == "--store" && ${COMP_CWORD} -eq 3 ]]; then
        COMPREPLY=( $(compgen -W "${opts} ${commands}" -- "${cur}") )
        return
    fi

    # Determine the subcommand
    local cmd=""
    for word in "${COMP_WORDS[@]:1}"; do
        if [[ "${commands}" =~ (^|[[:space:]])"${word}"($|[[:space:]]) ]]; then
            cmd="${word}"
            break
        fi
    done

    case "${cmd}" in
        show|info|edit|move|delete|allow|deny|list)
            # Complete passveil entries
            local entries
            if [[ -n "${store}" ]]; then
                entries=$(passveil --store "${store}" list 2>/dev/null)
            else
                entries=$(passveil list 2>/dev/null)
            fi
            COMPREPLY=( $(compgen -W "${entries}" -- "${cur}") )
            ;;
        insert)
            # Complete directories from passveil paths
            local paths
            if [[ -n "${store}" ]]; then
                paths=$(passveil --store "${store}" list 2>/dev/null | xargs -n1 dirname | sort -u)
            else
                paths=$(passveil list 2>/dev/null | xargs -n1 dirname | sort -u)
            fi
            COMPREPLY=( $(compgen -W "${paths}" -- "${cur}") )
            ;;
        move)
            # Complete destination paths
            if [[ "${prev}" == "${cmd}" ]]; then
                # Complete passveil entries
                local entries
                if [[ -n "${store}" ]]; then
                    entries=$(passveil --store "${store}" list 2>/dev/null)
                else
                    entries=$(passveil list 2>/dev/null)
                fi
                COMPREPLY=( $(compgen -W "${entries}" -- "${cur}") )
            else
                # Complete directories
                _filedir -d
            fi
            ;;
        allow|deny)
            # Complete GPG user IDs
            if [[ "${prev}" == "${cmd}" ]]; then
                # Complete passveil entries
                local entries
                if [[ -n "${store}" ]]; then
                    entries=$(passveil --store "${store}" list 2>/dev/null)
                else
                    entries=$(passveil list 2>/dev/null)
                fi
                COMPREPLY=( $(compgen -W "${entries}" -- "${cur}") )
            else
                # Complete GPG user IDs
                local gpg_ids=$(gpg --list-keys --with-colons 2>/dev/null | awk -F: '/^uid:/ {print $10}')
                COMPREPLY=( $(compgen -W "${gpg_ids}" -- "${cur}") )
            fi
            ;;
        init|distrust)
            # Complete GPG user IDs
            local gpg_ids=$(gpg --list-keys --with-colons 2>/dev/null | awk -F: '/^uid:/ {print $10}')
            COMPREPLY=( $(compgen -W "${gpg_ids}" -- "${cur}") )
            ;;
        *)
            ;;
    esac
}

complete -F _passveil passveil
