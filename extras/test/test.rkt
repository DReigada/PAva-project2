#lang racket
 
(require rackunit
         "../../preprocess.rkt"
         "../../preprocessextra.rkt")

(define input
#<<END
@Getters
@Setters
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


  public String getstr() {
      return this.str;
  }

  public int getnum() {
      return this.num;
  }


  public void setstr(String str) {
      this.str = str;
  }

  public void setnum(int num) {
      this.num = num;
  }
}
END
)

(check-equal? (process-string input) output "Java setters getters")


(define input-constructor
#<<END
@Constructor
public class Test {
    public String str;
    public int num;
    public int num2;
}
END
)

(define output-constructor
#<<END

public class Test {
    public String str;
    public int num;
    public int num2;

  public Test(String str, int num, int num2) {
    this.str = str;
    this.num = num;
    this.num2 = num2;
  }

}
END
)

(check-equal? (process-string input-constructor) output-constructor "Java constructor")
