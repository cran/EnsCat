tangle<-function(hc0,hc1){
	hcd0<-as.dendrogram(hc0)
	hcd1<-as.dendrogram(hc1)
	hcd0<-match_order_by_labels(hcd0,hcd1)
	dends_0_1 <- dendlist(hcd0, hcd1)
	t<-tanglegram(dends_0_1)
	t
}

