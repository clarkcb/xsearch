module gosearch

go 1.23.2

require (
	gofind v1.0.0
	golang.org/x/text v0.19.0
)

require github.com/pmylund/sortutil v0.0.0-20120526081524-abeda66eb583 // indirect

replace gofind v1.0.0 => ../../../xfind/go/gofind
