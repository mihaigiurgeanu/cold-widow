/* extract.c
 * defines wrapper function to extract the qr code from an quirc decoder
 */

#include "quirc.h"

/* Extract the text for a qr code or an error message.
 * @return 0 if the output text parameter contains the text in the qr code.
 * @return 1 if the output text contains an error message
 */
int extract_text(struct quirc *qr, int i, const char **text) {
  struct quirc_code code;
  struct quirc_data data;
  quirc_decode_error_t err;
  
  quirc_extract(qr, i, &code);
  
  /* Decoding stage */
  err = quirc_decode(&code, &data);
  if (err)
    *text = quirc_strerror(err);
  else
    *text = data.payload;

  return err?1:0;
}
