ghcid:
	ghcid -c "stack ghci mesa-verde --ghci-options -fno-code" --restart package.yaml

ghcid-test:
	ghcid -c "stack ghci mesa-verde:lib mesa-verde:test:specs --ghci-options -fobject-code" --restart package.yaml --test "main"

.PHONY: ghcid ghcid-test
