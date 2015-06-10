$(".collabLine").bind("click", function(){
    var a = $(this);
    var thisId = a.attr("id");
    showId(thisId);
});

$(".synopsis").hide();

function showId(i){
    $(".synopsis").hide();
    $("#"+i+".synopsis").show();
};
