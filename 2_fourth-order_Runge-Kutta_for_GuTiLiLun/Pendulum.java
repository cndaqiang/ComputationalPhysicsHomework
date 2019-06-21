///////////////////////////////////////////////////////////////////////////
//                                                                       //
// Program file name: Pendulum.java                                      //
//                                                                       //
// � Tao Pang 2006                                                       //
//                                                                       //
// Last modified: January 18, 2006                                       //
//                                                                       //
// (1) This Java program is part of the book, "An Introduction to        //
//     Computational Physics, 2nd Edition," written by Tao Pang and      //
//     published by Cambridge University Press on January 19, 2006.      //
//                                                                       //
// (2) No warranties, express or implied, are made for this program.     //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

// A program to study the driven pendulum under damping
// via the fourth-order Runge-Kutta algorithm.

// 采用了矩阵进行计算，这样可以同时计算g1,g2,y1,y2


import java.lang.*;
public class Pendulum {
  static final int n = 100, nt = 10, m = 5;
  public static void main(String argv[]) {
    double y1[] = new double[n+1]; //y1[] 存储计算过程中所有时刻的y1即y[0]
    double y2[] = new double[n+1]; //y2[] 存储计算过程中所有时刻的y2即y[1]
    double y[] = new double[2]; // y1=y[0], y2=y[1],为每一个时间点的y1和y2值

 // Set up time step and initial values
    double dt = 3*Math.PI/nt;
    y1[0] = y[0] = 0;
    y2[0] = y[1] = 2;

 // Perform the 4th-order Runge-Kutta integration
    for (int i=0; i<n; ++i) {
      double t = dt*i;
      y = rungeKutta(y, t, dt); 
      y1[i+1] = y[0];
      y2[i+1] = y[1];

   // Bring theta back to the region [-pi, pi]
      int np = (int) (y1[i+1]/(2*Math.PI)+0.5);
      y1[i+1] -= 2*Math.PI*np;
    }

 // Output the result in every m time steps
    for (int i=0; i<=n; i+=m) {
      System.out.println("Angle: " + y1[i]);
      System.out.println("Angular velocity: " + y2[i]);
      System.out.println();
    }
  }

// Method to complete one Runge-Kutta step.

  public static double[] rungeKutta(double y[],
    double t, double dt) {
    int l = y.length;
    double c1[] = new double[l];
    double c2[] = new double[l];
    double c3[] = new double[l];
    double c4[] = new double[l];
    c1 = g(y, t);
    for (int i=0; i<l; ++i) c2[i] = y[i] + dt*c1[i]/2;
    c2 = g(c2, t+dt/2);
    for (int i=0; i<l; ++i) c3[i] = y[i] + dt*c2[i]/2;
    c3 = g(c3, t+dt/2);
    for (int i=0; i<l; ++i) c4[i] = y[i] + dt*c3[i];
    c4 = g(c4, t+dt);
    for (int i=0; i<l; ++i)
      c1[i] = y[i] + dt*(c1[i]+2*(c2[i]+c3[i])+c4[i])/6;
    return c1;
  }

// Method to provide the generalized velocity vector.

  public static double[] g(double y[], double t) {
    int l = y.length;
    double q = 0.5, b = 0.9, omega0 = 2.0/3;
    double v[] = new double[l];
    v[0] = y[1];
    v[1] = -Math.sin(y[0])+b*Math.cos(omega0*t);
    v[1] -= q*y[1];
    return v;
  }
}
