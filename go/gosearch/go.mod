module gosearch

go 1.24.1

require (
	gofind v1.0.0
	golang.org/x/text v0.26.0
)

require github.com/pmylund/sortutil v0.0.0-20120526081524-abeda66eb583 // indirect

replace gofind v1.0.0 => ../../../xfind/go/gofind
