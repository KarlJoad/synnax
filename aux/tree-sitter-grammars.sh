if ! command -v recsel >/dev/null 2>&1; then
	echo "Could not find recsel!"
	echo "You must have recutils in \$PATH!"
	exit 1
fi

guix search tree-sitter-.+ | recsel -p name | awk '{print $2}' | grep -e '^tree-sitter-*' | uniq | sort
