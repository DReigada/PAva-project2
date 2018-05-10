#lang racket
 
(require rackunit
         "../../preprocess.rkt"
         "../../preprocessextra.rkt")

(define input
#<<END
@Setters
@Getters
public class Test {
    public String str;
    public int num;
}
END
)

(define output
#<<END


public class Test {
    public String str;
    public int num;


  public void setstr(String str) {
      this.str = str;
  }

  public void setnum(int num) {
      this.num = num;
  }


  public String getstr() {
      return this.str;
  }

  public int getnum() {
      return this.num;
  }
}
END
)

(check-equal? (process-string input) output "Java setters getters")
