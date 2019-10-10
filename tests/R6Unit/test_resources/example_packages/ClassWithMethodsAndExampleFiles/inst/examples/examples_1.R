
func1<- function(){
	# a comment in the example
        eci <- new(Class="ExposedClass",times=1:4)
	# another comment
        exposedGeneric(eci,1)}

func2<- function(){
        eci <- new(Class="ExposedClass",times=1:4)
        exposedGeneric(eci,2)
}
