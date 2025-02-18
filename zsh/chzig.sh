#!/usr/bin/env sh

function print_red()    { echo -e "\033[1;31m$1\033[0m" }
function print_green()  { echo -e "\033[1;32m$1\033[0m" }
function print_yellow() { echo -e "\033[1;33m$1\033[0m" }
function print_blue()   { echo -e "\033[1;34m$1\033[0m" }

function print_error() { echo "$(print_red chzig:)" $@ }
function print_info()  { echo "$(print_yellow chzig:)" $@ }
function print_run()   { echo "$(print_green chzig:)" $@ }

CHZIG_ROOT="$HOME/.local/chzig"

CHZIG_BIN="$CHZIG_ROOT/bin"
CHZIG_ZIG_INSTALLS="$CHZIG_ROOT/zig-installs"
CHZIG_DOWNLOADS="$CHZIG_ROOT/downloads"
CHZIG_ZLS_INSTALLS="$CHZIG_ROOT/zls-installs"

export PATH=$CHZIG_BIN:$PATH

function _chzig_download_path() {
    echo "https://ziglang.org/download/$1/zig-linux-x86_64-$1.tar.xz"
}

function _chzig_nightly_download_path() {
    print_error "need to manually specify url for nightly build"
}

function _chzig_already_downloaded() {
    [[ -d $CHZIG_ZIG_INSTALLS/$1 ]]
}

function install-chzig() {
    mkdir -p $CHZIG_BIN
    mkdir -p $CHZIG_ZIG_INSTALLS
    mkdir -p CHZIG_DOWNLOADS
    mkdir -p CHZIG_ZLS_INSTALLS

    echo "$(print_yellow chzig) installed successfully."
}

function uninstall-chzig() {
    rm -rf $CHZIG_ROOT
    echo "$(print_yellow chzig) uninstalled successfully."
}

function chzig() {
    case "$1" in
        help|"--help"|"-h")
            echo '
usage:
  chzig install 0.13.0    - download the zig version (for linux-x86_64)
  chzig ls                - list installed zig versions
  chzig use 0.13.0        - use the given version
'
            ;;
        *)
            if [[ ! -d $CHZIG_ZIG_INSTALLS ]]; then
                print_warn "chzig hasn't been initialized - run $(print_blue install-chzig) first"
            else
                case "$1" in
                    use)
                        local TARGET_VERSION=$2

                        if _chzig_already_downloaded $TARGET_VERSION; then
                            print_info "using $(print_blue $TARGET_VERSION)"

                            rm $CHZIG_BIN/zig
                            rm $CHZIG_BIN/zls

                            ln -s $CHZIG_ZIG_INSTALLS/$TARGET_VERSION/zig $CHZIG_BIN/zig
                            ln -s $CHZIG_ZLS_INSTALLS/$TARGET_VERSION/zls $CHZIG_BIN/zls
                        else
                            local msg="chzig install $TARGET_VERSION"
                            print_error "zig $TARGET_VERSION not downloaded yet - run $(print_blue $msg) first"
                        fi
                        ;;

                    install)
                        local TARGET_VERSION=$2

                        if [[ -f $CHZIG_DOWNLOADS/zig-"$TARGET_VERSION".tar.xz ]]; then
                            print_info "zig $(print_blue zig-$TARGET_VERSION.tar.xz) already has been downloaded"
                        else
                            print_run "downloading $(print_blue zig-$TARGET_VERSION.tar.xz) ..."
                            curl $(_chzig_download_path $TARGET_VERSION) -o $CHZIG_DOWNLOADS/zig-"$TARGET_VERSION".tar.xz
                        fi

                        local TARGET_DIR=$CHZIG_ZIG_INSTALLS/$TARGET_VERSION

                        mkdir -p $TARGET_DIR

                        if [[ -f $TARGET_DIR/zig ]]; then
                            print_info "zig $(print_blue $TARGET_VERSION) has already been installed"
                        else
                            print_run "installing zig $(print_blue $TARGET_VERSION)"
                            2>/dev/null 1>&2 tar -xvf $CHZIG_DOWNLOADS/zig-"$TARGET_VERSION".tar.xz -C $TARGET_DIR --strip-components=1
                        fi

                        if [[ -f $CHZIG_DOWNLOADS/zls-"$TARGET_VERSION".tar.xz ]]; then
                            print_info "zls $(print_blue zls-$TARGET_VERSION.tar.xz) has already been downloaded"
                        else
                            print_run "downloading $(print_blue zls-$TARGET_VERSION.tar.xz) ..."
                            curl https://github.com/zigtools/zls/releases/download/"$TARGET_VERSION"/zls-x86_64-linux.tar.gz -o $CHZIG_DOWNLOADS/zls-"$TARGET_VERSION".tar.xz
                        fi

                        local TARGET_DIR=$CHZIG_ZLS_INSTALLS/$TARGET_VERSION
                        mkdir -p $TARGET_DIR

                        if [[ -f $TARGET_DIR/zls ]]; then
                            print_info "zls $(print_blue $TARGET_VERSION) has already been installed"
                        else
                            print_run "installing zls $(print_blue $TARGET_VERSION)"
                            2>/dev/null 1>&2 tar -xvf $CHZIG_DOWNLOADS/zls-"$TARGET_VERSION".tar.xz -C $TARGET_DIR
                        fi

                        ;;

                    uninstall)
                        local TARGET_VERSION=$2
                        local TARGET_DIR=$CHZIG_ZIG_INSTALLS/$TARGET_VERSION

                        if [[ -d $TARGET_DIR ]]; then
                            print_run "uninstalling zig $(print_blue $TARGET_VERSION)"
                            rm -rf $TARGET_DIR
                        else
                            print_info "zig $(print_blue $TARGET_VERSION) is not currently installed"
                        fi
                        ;;

                    list|ls|status)
                        for dir in $CHZIG_ZIG_INSTALLS/*; do
                            local dir_version="${dir##*/}"
                            local zig_version=$(zig version)

                            if [[ "$dir_version" == "$zig_version" ]]; then
                                echo "$(print_blue "  ${dir##*/} <- current")"
                            else
                                echo "  ${dir##*/}"
                            fi
                        done
                        ;;

                    *)
                        print_error "unknown command: $1"
                        ;;
                esac
            fi
            ;;
    esac
}
