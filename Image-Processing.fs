
//
// F# image processing functions.
//
// Sania Khaja
// University of Illinois Chicago
// CS 341 Spring 2023
// 
// This code has the following 5 functions/operations: Grayscale, Threshold, FlipHorizontal, EdgeDetect, RotateRight90. Each of these functions will apply a change to the image based on the defined behavior of each function and return that updated image 
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // going through every row in image
    match image with
    | [] -> []  // base case
    | row :: rest ->  // row in image
      // going through each element and splitting that into r, g, b values. Ajdusts the color and creates a new tuple of that adjusted color. Adding new list of colors through recursion
      List.map(fun (r, g, b) -> 
      let color = int (0.299 * float r + 0.587 * float g + 0.114 * float b)
      (color, color, color)) row :: Grayscale width height depth rest
      


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
     // going through every row in image
    match image with
    | [] -> []  // base case
    | row :: rest ->  // row in image
      // going through each element and splitting that into r, g, b values. Ajdusts the color based on threshhold comparisons and creates a new tuple of that adjusted color. Adding new list of colors through recursion
      List.map(fun (r, g, b) -> 
      let c1 = if r > threshold then depth else 0
      let c2 = if g > threshold then depth else 0
      let c3 = if b > threshold then depth else 0
      (c1, c2, c3)) row :: Threshold width height depth rest threshold
      


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with 
    | [] -> []  // Base case: no image left to flip
    // goes through the image row by row. Flips the row by reversing it and then goes to next row
    | row :: restImage -> (List.rev row) :: (FlipHorizontal width height depth restImage)


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
                   (height:int)
                   (depth:int)
                   (image:(int*int*int) list list)
                   (threshold:int) =

    // Helper function to calculate the color distance between two pixels
    let distance ((r1,g1,b1):(int*int*int)) ((r2,g2,b2):(int*int*int)) =
        let r_diff = pown (r1 - r2) 2
        let g_diff = pown (g1 - g2) 2
        let b_diff = pown (b1 - b2) 2
        sqrt (float (r_diff + g_diff + b_diff))

    // Helper function to check if a pixel is an edge pixel
    let is_edge_pixel (x:int) (y:int) =
        let p = List.item x (List.item y image)
        let p_right = if x < width-1 then List.item (x+1) (List.item y image) else p
        let p_bottom = if y < height-1 then List.item x (List.item (y+1) image) else p
        let d1 = distance p p_right
        let d2 = distance p p_bottom
        d1 > float threshold || d2 > float threshold
    
    // Iterate over the input image and replace each pixel with a black or white pixel
    let updatedImage =
        List.init (height-1) (fun y ->
            List.init (width-1) (fun x ->
                if is_edge_pixel x y then (0,0,0)
                else (255,255,255)))
    updatedImage
    
  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // transposing list so switching rows and columns. row 0 switches with col 0 and so on
    let transposed = List.transpose image
    // reversing each row since transposing would rotate left instead of right. Would replicate row 0 being switched with last col
    let reversed = List.map List.rev transposed
    // returning rotated image
    reversed
      
