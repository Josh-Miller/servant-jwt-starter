help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid-devel: ## Run the server in fast development mode. See DevelMain for details.
	ghcid \
	    --command "stack ghci payment" \
	    --test "DevelMain.update"

.PHONY: ghcid-devel help
